-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}

module PhiElimination where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')
import Control.Monad (foldM, forM)
import Data.Maybe (fromMaybe)
import Control.Monad.State

import IR

-- State to generate fresh temporary registers during Phi lowering
type PhiGen a = State Integer a

eliminatePhis :: ProgramIR -> ProgramIR
eliminatePhis prog = 
    let 
        -- Calculate max VReg to initialize state for fresh temps
        maxV = maxVRegProgram prog
        (noPhisProg, _) = runState (lowerPhis prog) (maxV + 1)
    in
        -- Phase 2 (Coalescing) is REMOVED because it was unsafe.
        -- The Register Allocator will handle safe coalescing.
        noPhisProg

maxVRegProgram :: ProgramIR -> Integer
maxVRegProgram (ProgramIR blocks) = Map.foldr checkBlock 0 blocks
  where
    checkBlock b m = foldr checkInstr m (blockInsns b)
    checkInstr (Move (VReg v) _) m = max v m
    checkInstr (Compute _ (VReg v) _ _) m = max v m
    checkInstr (Load (VReg v) _) m = max v m
    checkInstr (LoadIndirect (VReg v) _) m = max v m
    checkInstr _ m = m

-- ===========================================================================
-- Phase 1: Lower Phis with Parallel Copy Safety
-- ===========================================================================

lowerPhis :: ProgramIR -> PhiGen ProgramIR
lowerPhis (ProgramIR blockMap) = do
    let movesMap = collectPhiMoves blockMap
    finalBlockMap <- foldM applyMoves blockMap (Map.toList movesMap)
    let cleanedBlockMap = Map.map clearPhis finalBlockMap
    return $ ProgramIR cleanedBlockMap

collectPhiMoves :: Map Label Block -> Map (Label, Label) [(VReg, Operand)]
collectPhiMoves blocks = 
    Map.foldrWithKey scanBlock Map.empty blocks
  where
    scanBlock :: Label -> Block -> Map (Label, Label) [(VReg, Operand)] -> Map (Label, Label) [(VReg, Operand)]
    scanBlock succLbl blk acc = 
        foldl' (accumulateMoves succLbl) acc (blockPhis blk)

    accumulateMoves :: Label -> Map (Label, Label) [(VReg, Operand)] -> Phi -> Map (Label, Label) [(VReg, Operand)]
    accumulateMoves succLbl accMap (Phi vDest sources) = 
        foldl' (\m (predLbl, srcOp) -> 
            let key = (predLbl, succLbl)
            in Map.insertWith (++) key [(vDest, srcOp)] m
        ) accMap sources

applyMoves :: Map Label Block -> ((Label, Label), [(VReg, Operand)]) -> PhiGen (Map Label Block)
applyMoves blocks ((predLbl, succLbl), assignments) = do
    -- Generate safe sequence of moves using temporaries to handle swaps/cycles.
    safeMoves <- generateSafeMoves assignments

    case Map.lookup predLbl blocks of
        Nothing -> error $ "Phi predecessor block not found: " ++ T.unpack predLbl
        Just predBlk ->
            case blockTerm predBlk of
                Jump target | target == succLbl -> do
                    let newBlk = predBlk { blockInsns = blockInsns predBlk ++ safeMoves }
                    return $ Map.insert predLbl newBlk blocks
                
                Branch cond v1 op t f | t == succLbl || f == succLbl -> do
                    let 
                        splitLbl = predLbl <> "_split_" <> succLbl
                        splitBlk = Block
                            { blockLabel = splitLbl
                            , blockArgs  = [] 
                            , blockPhis  = []
                            , blockInsns = safeMoves
                            , blockTerm  = Jump succLbl
                            }
                        newT = if t == succLbl then splitLbl else t
                        newF = if f == succLbl then splitLbl else f
                        newTerm = Branch cond v1 op newT newF
                        newPredBlk = predBlk { blockTerm = newTerm }
                    return $ Map.insert splitLbl splitBlk (Map.insert predLbl newPredBlk blocks)

                _ -> return blocks 

generateSafeMoves :: [(VReg, Operand)] -> PhiGen [Middle]
generateSafeMoves assignments = do
    -- 1. Read all sources into fresh temporaries
    tempReads <- forM assignments $ \(_, srcOp) -> do
        tmp <- freshTemp
        return (tmp, srcOp) -- Move tmp srcOp
    
    -- 2. Write temporaries to destinations
    let tempWrites = zipWith (\(vDest, _) (tmp, _) -> (vDest, OpReg tmp)) assignments tempReads

    let readInstrs  = map (\(t, s) -> Move t s) tempReads
    let writeInstrs = map (\(d, s) -> Move d s) tempWrites
    
    return $ readInstrs ++ writeInstrs

freshTemp :: PhiGen VReg
freshTemp = do
    c <- get
    put (c + 1)
    return (VReg c)

clearPhis :: Block -> Block
clearPhis b = b { blockPhis = [] }