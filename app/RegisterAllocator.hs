{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RegisterAllocator (allocate) where

import PseudoInstructions
import VMInterface
import IR (VReg(..))


import IR (Label) 
import qualified Data.Map as Map

import Data.List (sort, sortBy, nub, maximumBy)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Configuration
-- ============================================================================

allocatableRegs :: [Register]
allocatableRegs = [RegB, RegC, RegD, RegE, RegF, RegG]

scratchReg :: Register
scratchReg = RegH

type Line = Int
type Interval = (Line, Line) 
type LiveIntervals = Map VReg Interval
type PreColoredIntervals = Map Register [Interval] 
type Coloring = Map VReg Register

-- ============================================================================
-- Analysis
-- ============================================================================

getDefs :: Pseudo -> [PReg]
getDefs p = case p of
    P_RSTORE r  -> [r]
    P_SWP r     -> [r]
    P_RST r     -> [r]
    P_INC r     -> [r]
    P_DEC r     -> [r]
    P_SHL r     -> [r]
    P_SHR r     -> [r]
    _           -> []

getUses :: Pseudo -> [PReg]
getUses p = case p of
    P_RLOAD r   -> [r]
    P_ADD r     -> [r]
    P_SUB r     -> [r]
    -- P_SWP r     -> [r]
    P_INC r     -> [r]
    P_DEC r     -> [r]
    P_SHL r     -> [r]
    P_SHR r     -> [r]
    P_LOAD (T_VReg v)  -> [Left v]
    P_STORE (T_VReg v) -> [Left v]
    P_JUMP (T_VReg v)  -> [Left v]
    P_JPOS (T_VReg v)  -> [Left v]
    P_JZERO (T_VReg v) -> [Left v]
    P_CALL (T_VReg v)  -> [Left v]
    _                  -> []

-- Helper to extract label name from instruction (Assuming string labels)
getLabel :: Pseudo -> Maybe Label
getLabel (P_LABEL lbl) = Just lbl
getLabel _ = Nothing

-- | Helper to extract jump target label
getJumpTarget :: Pseudo -> Maybe Label
getJumpTarget p = case p of
    P_JUMP (T_Label l)  -> Just l
    P_JPOS (T_Label l)  -> Just l
    P_JZERO (T_Label l) -> Just l
    P_CALL (T_Label l)  -> Just l -- Good to include CALLs if you handle recursion loops
    _ -> Nothing

-- ============================================================================
-- Interprocedural Liveness Analysis
-- ============================================================================

-- ============================================================================
-- Intra-procedural Liveness Analysis (Loop Aware, Call Isolated)
-- ============================================================================

buildLiveIntervals :: [Pseudo] -> LiveIntervals
buildLiveIntervals instrs = 
    let 
        n = length instrs
        -- 1. Build Control Flow Graph (Adjacency List)
        -- Map: Line -> [Successor Lines]
        labelMap = Map.fromList [ (lbl, i) | (i, p) <- zip [0..] instrs, Just lbl <- [getLabel p] ]
        
        getSuccs :: Int -> Pseudo -> [Int]
        getSuccs i p = case p of
            -- Loops/Branches: Follow the flow
            P_JUMP (T_Label l)  -> maybe [] return (Map.lookup l labelMap)
            P_JPOS (T_Label l)  -> maybe [] (\t -> [t, i+1]) (Map.lookup l labelMap)
            P_JZERO (T_Label l) -> maybe [] (\t -> [t, i+1]) (Map.lookup l labelMap)
            
            -- Function Calls: Treat as linear instruction.
            -- We DO NOT follow the label. We assume flow continues to next line (after return).
            -- The 'clobber' in buildPreColored handles the register pressure.
            P_CALL _            -> if i + 1 < n then [i+1] else []
            
            P_RTRN              -> [] -- End of flow for this function
            P_HALT              -> []
            _                   -> if i + 1 < n then [i+1] else []

        cfg :: Map Int [Int]
        cfg = Map.fromList [ (i, getSuccs i p) | (i, p) <- zip [0..] instrs ]

        -- 2. Compute Def/Use sets
        defUse :: Map Int (Set VReg, Set VReg)
        defUse = Map.fromList 
            [ (i, (Set.fromList [v | Left v <- getDefs p], Set.fromList [v | Left v <- getUses p])) 
            | (i, p) <- zip [0..] instrs ]

        -- 3. Iterative Solver (Standard Data Flow)
        solve :: Map Int (Set VReg) -> Map Int (Set VReg)
        solve currentLiveIn = 
            let 
                step i = 
                    let (defs, uses) = defUse Map.! i
                        succs = cfg Map.! i
                        liveOut = Set.unions [ Map.findWithDefault Set.empty s currentLiveIn | s <- succs ]
                        newLiveIn = Set.union uses (Set.difference liveOut defs)
                    in (i, newLiveIn)
                
                nextLiveIn = Map.fromList (map step [0 .. n-1])
            in 
            if nextLiveIn == currentLiveIn 
            then nextLiveIn 
            else solve nextLiveIn

        finalLiveIn = solve (Map.fromList [(i, Set.empty) | i <- [0..n-1]])

        -- 4. Convert Liveness Sets to Intervals
        updateIntervals acc (i, liveSet) = 
            Set.foldr (\v m -> Map.insertWith unionRange v (i, i) m) acc liveSet
        
        addDefs acc (i, (defs, _)) = 
            Set.foldr (\v m -> Map.insertWith unionRange v (i, i) m) acc defs

        unionRange (s1, e1) (s2, e2) = (min s1 s2, max e1 e2)

        intervalsFromLiveIn = foldl updateIntervals Map.empty (Map.toList finalLiveIn)
        finalIntervals = foldl addDefs intervalsFromLiveIn (Map.toList defUse)

    in finalIntervals

-- ============================================================================
-- THE FIX IS HERE (buildPreColored)
-- ============================================================================

buildPreColored :: [Pseudo] -> PreColoredIntervals
buildPreColored instrs = foldl update Map.empty (zip [0..] instrs)
  where
    update acc (line, instr) =
        let 
            explicitRegs = [r | Right r <- getDefs instr ++ getUses instr]
            
            -- FIX: Restore Clobbering.
            -- P_CALL kills all allocatable registers.
            -- This forces the allocator to spill any variable live across this instruction,
            -- leaving the registers clean for the called function.
            clobberedRegs = case instr of
                P_CALL _ -> allocatableRegs 
                _        -> []
            
            allRegs = explicitRegs ++ clobberedRegs
        in 
        foldl (\m r -> Map.insertWith (++) r [(line, line)] m) acc allRegs

-- ============================================================================
-- Allocation Strategy (Linear Scan)
-- ============================================================================

linearScan :: LiveIntervals -> PreColoredIntervals -> (Coloring, [VReg])
linearScan intervals preColored = go sortedIntervals [] Map.empty []
  where
    sortedIntervals = sortBy (comparing (fst . snd)) (Map.toList intervals)

    go [] _ coloring spilled = (coloring, spilled)
    go ((v, (start, end)) : rest) active coloring spilled =
        let 
            stillActive = filter (\(_, _, (_, e)) -> e >= start) active
            vregInterference = Set.fromList [r | (_, r, _) <- stillActive]

            isPhysicallyBusy r = case Map.lookup r preColored of
                Nothing -> False
                Just ranges -> any (overlaps (start, end)) ranges

            overlaps (s1, e1) (s2, e2) = s1 <= e2 && s2 <= e1

            candidates = filter (\r -> 
                not (Set.member r vregInterference) && 
                not (isPhysicallyBusy r)
                ) allocatableRegs
        in
        case candidates of
            (r:_) -> go rest ((v, r, (start, end)) : stillActive) (Map.insert v r coloring) spilled
            [] -> 
                let candidate = (v, (start, end))
                    activeCandidates = map (\(av, ar, aint) -> (av, aint)) stillActive
                    (victimV, _) = maximumBy (comparing (snd . snd)) (candidate : activeCandidates)
                in
                if victimV == v then
                    go rest stillActive coloring (v : spilled)
                else
                    let (_, victimReg, _) = head $ filter (\(x,_,_) -> x == victimV) stillActive
                        newActive = filter (\(x,_,_) -> x /= victimV) stillActive
                        finalActive = (v, victimReg, (start, end)) : newActive
                        newColoring = Map.insert v victimReg coloring
                    in
                    go rest finalActive newColoring (victimV : spilled)

-- ============================================================================
-- Main & Rewriting
-- ============================================================================

allocate :: [Pseudo] -> [Pseudo]
allocate instrs = allocateStep 0 instrs
  where
    allocateStep depth currentCode 
        | depth > 50 = error "Register Allocation infinite loop (Spill thrashing)"
        | otherwise = 
            let 
                intervals = buildLiveIntervals currentCode
                preColored = buildPreColored currentCode
                (coloring, spilled) = linearScan intervals preColored
            in
            if null spilled
            then applyColoring coloring currentCode
            else 
                let newCode = rewriteSpills spilled currentCode
                in allocateStep (depth + 1) newCode

applyColoring :: Coloring -> [Pseudo] -> [Pseudo]
applyColoring coloring = map replace
  where
    reg :: PReg -> PReg
    reg (Right r) = Right r 
    reg (Left v)  = case Map.lookup v coloring of
        Just r  -> Right r
        Nothing -> error $ "VReg " ++ show v ++ " uncolored"

    tgt (T_VReg v) = T_VReg v 
    tgt t = t

    replace p = case p of
        P_RLOAD r  -> P_RLOAD (reg r)
        P_RSTORE r -> P_RSTORE (reg r)
        P_ADD r    -> P_ADD (reg r)
        P_SUB r    -> P_SUB (reg r)
        P_SWP r    -> P_SWP (reg r)
        P_RST r    -> P_RST (reg r)
        P_INC r    -> P_INC (reg r)
        P_DEC r    -> P_DEC (reg r)
        P_SHL r    -> P_SHL (reg r)
        P_SHR r    -> P_SHR (reg r)
        P_LOAD t   -> P_LOAD (tgt t)
        P_STORE t  -> P_STORE (tgt t)
        P_JUMP t   -> P_JUMP (tgt t)
        P_JPOS t   -> P_JPOS (tgt t)
        P_JZERO t  -> P_JZERO (tgt t)
        P_CALL t   -> P_CALL (tgt t)
        other      -> other

rewriteSpills :: [VReg] -> [Pseudo] -> [Pseudo]
rewriteSpills spilledVars instrs = concatMap process instrs
  where
    maxAddr :: Integer
    maxAddr = (2^62) - 1

    uniqueSpills = nub spilledVars
    spillMap = Map.fromList $ zip uniqueSpills [maxAddr, maxAddr - 1 ..]

    process instr = 
        let defs = [v | Left v <- getDefs instr]
            uses = [v | Left v <- getUses instr]
            isSpilled v = Map.member v spillMap
        in if any isSpilled (defs ++ uses)
           then expandSpillInstr instr spillMap
           else [instr]

expandSpillInstr :: Pseudo -> Map VReg Address -> [Pseudo]
expandSpillInstr instr mapping = 
    let look v = Map.lookup v mapping
    in case instr of
        -- 1. Indirect Store: Mem[v] <- RegA
        -- v is the POINTER (spilled).
        P_RSTORE (Left v) | Just addr <- look v -> 
            [ P_SWP (Right scratchReg)      -- Save A to H
            , P_LOAD (T_Addr addr)          -- Load Pointer v to A
            , P_SWP (Right scratchReg)      -- A = Val, H = Pointer
            , P_RSTORE (Right scratchReg)   -- Mem[H] = A
            ]

        -- 2. Indirect Load: RegA <- Mem[v]
        -- v is the POINTER (spilled).
        P_RLOAD (Left v) | Just addr <- look v ->
            [ P_LOAD (T_Addr addr)          -- Load Pointer v to A
            , P_SWP (Right scratchReg)      -- Move Pointer to H
            , P_RLOAD (Right scratchReg)    -- A = Mem[H]
            ]

        -- 3. Simple Moves (Assigning to/from the variable itself)
        P_STORE (T_VReg v) | Just addr <- look v -> [P_STORE (T_Addr addr)]
        P_LOAD (T_VReg v)  | Just addr <- look v -> [P_LOAD (T_Addr addr)]
        
        -- 4. Math: A = A + v
        -- P_ADD (Left v) | Just addr <- look v -> 
        --     [ P_SWP (Right scratchReg)      -- H = Acc (A)
        --     , P_LOAD (T_Addr addr)          -- A = v
        --     , P_ADD (Right scratchReg)      -- A = v + Acc
        --     -- Result is in A. H is garbage (contains v or Acc depending on HW), but unused.
        --     ]
            
        -- -- 5. Math: A = A - v
        -- P_SUB (Left v) | Just addr <- look v -> 
        --     [ P_SWP (Right scratchReg)      -- H = Acc (A)
        --     , P_LOAD (T_Addr addr)          -- A = v
        --     , P_SWP (Right scratchReg)      -- A = Acc, H = v
        --     , P_SUB (Right scratchReg)      -- A = Acc - v
        --     ]
        P_ADD (Left v) | Just addr <- look v -> 
            [ P_SWP (Right scratchReg)      -- 1. Save current A (Accumulator) in H
            , P_LOAD (T_Addr addr)          -- 2. Load v (the operand) from stack to A
            , P_SWP (Right scratchReg)      -- 3. Now A = Accumulator, H = v
            , P_ADD (Right scratchReg)      -- 4. A = A + H
            ]
            
        -- 5. Math: A = A - v (v is spilled)
        P_SUB (Left v) | Just addr <- look v -> 
            [ P_SWP (Right scratchReg)      -- 1. Save current A in H
            , P_LOAD (T_Addr addr)          -- 2. Load v from stack to A
            , P_SWP (Right scratchReg)      -- 3. A = Accumulator, H = v
            , P_SUB (Right scratchReg)      -- 4. A = A - H
            ]
        -- 6. Unary: v++ / v--
        P_INC (Left v) | Just addr <- look v ->
            [ P_SWP (Right scratchReg)      -- Save A to H
            , P_LOAD (T_Addr addr)          -- Load v
            , P_INC (Right RegA)            -- Increment v
            , P_STORE (T_Addr addr)         -- Store v
            , P_SWP (Right scratchReg)      -- Restore A
            ]

        P_DEC (Left v) | Just addr <- look v ->
            [ P_SWP (Right scratchReg)
            , P_LOAD (T_Addr addr)
            , P_DEC (Right RegA)
            , P_STORE (T_Addr addr)
            , P_SWP (Right scratchReg)
            ]
        
        -- 7. Swap: A <-> v
        P_SWP (Left v) | Just addr <- look v ->
            [ P_SWP (Right scratchReg)      -- 1. H = NewVal(A), A = OldH
            , P_LOAD (T_Addr addr)          -- 2. A = OldVal(v)
            , P_SWP (Right scratchReg)      -- 3. A = NewVal, H = OldVal
            , P_STORE (T_Addr addr)         -- 4. Store NewVal to v
            , P_SWP (Right scratchReg)      -- 5. A = OldVal, H = NewVal (Garbage)
            ]
            
        _ -> [instr]