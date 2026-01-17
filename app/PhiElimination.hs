-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}

module PhiElimination where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')
import Data.Maybe (fromMaybe)

import IR

eliminatePhis :: ProgramIR -> ProgramIR
eliminatePhis prog = 
    let 
        noPhisProg = lowerPhis prog
        optimizedProg = coalesceMoves noPhisProg
    in
        optimizedProg

-- ... (Phase 1 LowerPhis is identical to previous) ...
lowerPhis :: ProgramIR -> ProgramIR
lowerPhis (ProgramIR blockMap) = 
    let 
        movesMap = collectPhiMoves blockMap
        finalBlockMap = foldl' applyMoves blockMap (Map.toList movesMap)
        cleanedBlockMap = Map.map clearPhis finalBlockMap
    in
        ProgramIR cleanedBlockMap

collectPhiMoves :: Map Label Block -> Map (Label, Label) [Middle]
collectPhiMoves blocks = 
    Map.foldrWithKey scanBlock Map.empty blocks
  where
    scanBlock :: Label -> Block -> Map (Label, Label) [Middle] -> Map (Label, Label) [Middle]
    scanBlock succLbl blk acc = 
        foldl' (accumulateMoves succLbl) acc (blockPhis blk)

    accumulateMoves :: Label -> Map (Label, Label) [Middle] -> Phi -> Map (Label, Label) [Middle]
    accumulateMoves succLbl accMap (Phi vDest sources) = 
        foldl' (\m (predLbl, srcOp) -> 
            let move = Move vDest srcOp
                key = (predLbl, succLbl)
            in Map.insertWith (++) key [move] m
        ) accMap sources

applyMoves :: Map Label Block -> ((Label, Label), [Middle]) -> Map Label Block
applyMoves blocks ((predLbl, succLbl), moves) = 
    case Map.lookup predLbl blocks of
        Nothing -> error $ "Phi predecessor block not found: " ++ T.unpack predLbl
        Just predBlk ->
            case blockTerm predBlk of
                Jump target | target == succLbl ->
                    let newBlk = predBlk { blockInsns = blockInsns predBlk ++ moves }
                    in Map.insert predLbl newBlk blocks
                
                Branch cond v1 op t f | t == succLbl || f == succLbl ->
                    let 
                        splitLbl = predLbl <> "_split_" <> succLbl
                        splitBlk = Block
                            { blockLabel = splitLbl
                            , blockArgs  = [] 
                            , blockPhis  = []
                            , blockInsns = moves
                            , blockTerm  = Jump succLbl
                            }
                        newT = if t == succLbl then splitLbl else t
                        newF = if f == succLbl then splitLbl else f
                        newTerm = Branch cond v1 op newT newF
                        newPredBlk = predBlk { blockTerm = newTerm }
                    in
                        Map.insert splitLbl splitBlk (Map.insert predLbl newPredBlk blocks)

                _ -> blocks 

clearPhis :: Block -> Block
clearPhis b = b { blockPhis = [] }

-- ===========================================================================
-- Phase 2: Variable Coalescing with Chain Resolution
-- ===========================================================================

coalesceMoves :: ProgramIR -> ProgramIR
coalesceMoves (ProgramIR blocks) = 
    let
        renames = Map.foldr findMoves Map.empty blocks
        
        -- Logic: Map vSrc -> vDest.
        -- If v1 -> v2, and we see Move v2 (OpReg v1), we record v1 -> v2.
        -- We need to ensure transitive closure.
        findMoves :: Block -> Map VReg VReg -> Map VReg VReg
        findMoves blk m = foldl' checkInstr m (blockInsns blk)
        
        checkInstr m (Move vDest (OpReg vSrc)) 
            | vDest /= vSrc = 
                -- We are replacing vSrc with vDest.
                -- 1. Check if vDest is already mapped to something (chaining forward)
                let finalDest = Map.findWithDefault vDest vDest m
                    
                    -- 2. Insert vSrc -> finalDest
                    m' = Map.insert vSrc finalDest m
                    
                    -- 3. Path Compression: Update all existing keys pointing to vSrc
                    -- If we had vX -> vSrc, now vSrc -> finalDest, so update vX -> finalDest
                    m'' = Map.map (\v -> if v == vSrc then finalDest else v) m'
                in m''
        checkInstr m _ = m

        renamedBlocks = Map.map (renameBlock renames) blocks
    in
        ProgramIR renamedBlocks

renameBlock :: Map VReg VReg -> Block -> Block
renameBlock env b = 
    b { blockArgs  = map rV (blockArgs b)
      , blockInsns = filter (not . isSelfMove) $ map rI (blockInsns b)
      , blockTerm  = rT (blockTerm b)
      }
  where
    rV :: VReg -> VReg
    rV v = fromMaybe v (Map.lookup v env)

    rOp :: Operand -> Operand
    rOp (OpReg v) = OpReg (rV v)
    rOp x = x

    isSelfMove :: Middle -> Bool
    isSelfMove (Move v1 (OpReg v2)) = v1 == v2
    isSelfMove _ = False

    rI :: Middle -> Middle
    rI (Move v op)          = Move (rV v) (rOp op)
    rI (Compute o v o1 o2)  = Compute o (rV v) (rOp o1) (rOp o2)
    rI (Load v a)           = Load (rV v) a
    rI (Store a op)         = Store a (rOp op)
    rI (LoadIndirect v p)   = LoadIndirect (rV v) (rV p)
    rI (StoreIndirect p op) = StoreIndirect (rV p) (rOp op)
    rI (Print v)            = Print (rV v)
    rI (ReadInput v)        = ReadInput (rV v)
    rI (Call l ops)         = Call l (map rOp ops)
    rI (StoreRetAddr v)     = StoreRetAddr (rV v)
    rI (LoadRetAddr v)      = LoadRetAddr (rV v)
    rI x = x

    rT :: Terminator -> Terminator
    rT (Branch c v op l1 l2) = Branch c (rV v) (rOp op) l1 l2
    rT x = x