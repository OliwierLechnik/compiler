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

getUses :: Pseudo -> [PReg]
getUses p = case p of

    P_RSTORE r          -> [r, Right RegA] -- FIX: RSTORE reads pointer
    P_SWP (Left _)      -> [Right RegA]    -- FIX: Virtual SWP is Def-Only (Write)
    P_SWP (Right r)     -> [Right r, Right RegA] -- Phys SWP is Read-Write

    P_RLOAD r           -> [r]
    P_ADD r             -> [r, Right RegA]
    P_SUB r             -> [r, Right RegA]
    P_INC r             -> [r]
    P_DEC r             -> [r]
    P_SHL r             -> [r]
    P_SHR r             -> [r]
    P_STORE _           -> [Right RegA]
    P_WRITE             -> [Right RegA]
    P_JPOS _            -> [Right RegA]
    P_JZERO _           -> [Right RegA]
    P_RTRN              -> [Right RegA]
    P_JUMP (T_VReg v)   -> [Left v]
    P_JPOS (T_VReg v)   -> [Left v]
    P_JZERO (T_VReg v)  -> [Left v]
    P_CALL (T_VReg v)   -> [Left v]
    _                   -> []

getDefs :: Pseudo -> [PReg]
getDefs p = case p of
    -- SWP always Defines (Overwrites) both operands
    P_SWP r     -> [r, Right RegA]
    P_RST r     -> [r]
    P_INC r     -> [r]
    P_DEC r     -> [r]
    P_SHL r     -> [r]
    P_SHR r     -> [r]
    P_RLOAD _   -> [Right RegA]
    P_LOAD _    -> [Right RegA]
    P_ADD _     -> [Right RegA]
    P_SUB _     -> [Right RegA]
    P_READ      -> [Right RegA]
    _           -> []

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

-- Global constant
maxAddr :: Integer
maxAddr = (2^62) - 1

allocate :: [Pseudo] -> [Pseudo]
allocate instrs = allocateStep 0 maxAddr instrs
  where
    allocateStep depth currentSpillAddr currentCode 
        | depth > 50 = error "Spill thrashing"
        | otherwise = 
            let 
                intervals = buildLiveIntervals currentCode
                preColored = buildPreColored currentCode
                (coloring, spilled) = linearScan intervals preColored
            in
            if null spilled
            then applyColoring coloring currentCode
            else 
                let (nextSpillAddr, newCode) = rewriteSpills currentSpillAddr spilled currentCode
                in allocateStep (depth + 1) nextSpillAddr newCode

-- Update rewriteSpills signature
-- In RegisterAllocator.hs

rewriteSpills :: Integer -> [VReg] -> [Pseudo] -> (Integer, [Pseudo])
rewriteSpills currentSpillAddr spilledVars instrs = (nextSpillAddr, newInstrs)
  where
    -- 1. Calculate new addresses for this batch of spills
    uniqueSpills = nub spilledVars
    spillCount = fromIntegral (length uniqueSpills)
    
    -- Assign addresses descending: 900, 899, 898...
    assignedAddrs = [currentSpillAddr, currentSpillAddr - 1 ..]
    spillMap = Map.fromList $ zip uniqueSpills assignedAddrs
    
    -- 2. Update the counter for the next pass so it doesn't overlap
    nextSpillAddr = currentSpillAddr - spillCount

    -- 3. Rewrite the instructions
    newInstrs = concatMap (process spillMap) instrs

    -- ====================================================================
    -- This is the helper function that was missing
    -- ====================================================================
    process :: Map VReg Address -> Pseudo -> [Pseudo]
    process mapping instr = 
        let 
            -- We must check if any variable used/defined in this instruction 
            -- is in our spill map.
            defs = [v | Left v <- getDefs instr]
            uses = [v | Left v <- getUses instr]
            
            isSpilled v = Map.member v mapping
        in 
            if any isSpilled (defs ++ uses)
            then expandSpillInstr instr mapping -- Call the expansion logic
            else [instr]                        -- Keep instruction as is

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



-- In RegisterAllocator.hs

expandSpillInstr :: Pseudo -> Map VReg Address -> [Pseudo]
expandSpillInstr instr mapping = 
    let look v = Map.lookup v mapping
    in case instr of
        -- 1. Indirect Store: Mem[v] <- RegA
        P_RSTORE (Left v) | Just addr <- look v -> 
            [ P_SWP (Right scratchReg)      -- Save A to H
            , P_LOAD (T_Addr addr)          -- Load Pointer v to A
            , P_SWP (Right scratchReg)      -- A = Val, H = Pointer
            , P_RSTORE (Right scratchReg)   -- Mem[H] = A
            ]

        -- 2. Indirect Load: RegA <- Mem[v]
        P_RLOAD (Left v) | Just addr <- look v ->
            [ P_LOAD (T_Addr addr)          -- Load Pointer v to A
            , P_SWP (Right scratchReg)      -- Move Pointer to H
            , P_RLOAD (Right scratchReg)    -- A = Mem[H]
            ]

        -- 3. Simple Moves (Memory Access)
        P_STORE (T_VReg v) | Just addr <- look v -> [P_STORE (T_Addr addr)]
        P_LOAD (T_VReg v)  | Just addr <- look v -> [P_LOAD (T_Addr addr)]
        
        -- 4. Math: A = A + v
        P_ADD (Left v) | Just addr <- look v -> 
            [ P_SWP (Right scratchReg)      -- Save A
            , P_LOAD (T_Addr addr)          -- Load v
            , P_SWP (Right scratchReg)      -- Restore A, H=v
            , P_ADD (Right scratchReg)      -- A + H
            ]
            
        -- 5. Math: A = A - v
        P_SUB (Left v) | Just addr <- look v -> 
            [ P_SWP (Right scratchReg)      -- Save A
            , P_LOAD (T_Addr addr)          -- Load v
            , P_SWP (Right scratchReg)      -- Restore A, H=v
            , P_SUB (Right scratchReg)      -- A - H
            ]

        -- 6. Read-Modify-Write Unary: INC, DEC
        P_INC (Left v) | Just addr <- look v ->
            [ P_SWP (Right scratchReg)      -- Save A
            , P_LOAD (T_Addr addr)          -- Load v
            , P_INC (Right RegA)            -- Op
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

        -- 7. MISSING: Bitwise Shifts (SHL, SHR)
        -- These were causing your thrashing loop!
        P_SHL (Left v) | Just addr <- look v ->
            [ P_SWP (Right scratchReg)      -- Save A
            , P_LOAD (T_Addr addr)          -- Load v
            , P_SHL (Right RegA)            -- Op (Shift A)
            , P_STORE (T_Addr addr)         -- Store v
            , P_SWP (Right scratchReg)      -- Restore A
            ]

        P_SHR (Left v) | Just addr <- look v ->
            [ P_SWP (Right scratchReg)      -- Save A
            , P_LOAD (T_Addr addr)          -- Load v
            , P_SHR (Right RegA)            -- Op (Shift A)
            , P_STORE (T_Addr addr)         -- Store v
            , P_SWP (Right scratchReg)      -- Restore A
            ]

        -- 8. MISSING: Reset (RST)
        P_RST (Left v) | Just addr <- look v ->
            [ P_SWP (Right scratchReg)      -- Save A (we need to preserve A's value)
            , P_RST (Right RegA)            -- A = 0
            , P_STORE (T_Addr addr)         -- Mem[v] = 0
            , P_SWP (Right scratchReg)      -- Restore A
            ]

        -- 9. Swap: A <-> v
        P_SWP (Left v) | Just addr <- look v ->
            [ P_SWP (Right scratchReg)      -- H = NewVal(A), A = OldH
            , P_LOAD (T_Addr addr)          -- A = OldVal(v)
            , P_SWP (Right scratchReg)      -- A = NewVal, H = OldVal
            , P_STORE (T_Addr addr)         -- Store NewVal to v
            , P_SWP (Right scratchReg)      -- A = OldVal, H = NewVal
            ]
            
        -- Fallback: If it's a VReg that ISN'T in the map, keep it.
        -- But if it IS in the map and we didn't match above, it's a fatal error.
        _ -> 
            let defs = [v | Left v <- getDefs instr]
                uses = [v | Left v <- getUses instr]
                spilledHere = filter (\v -> Map.member v mapping) (defs ++ uses)
            in
            if null spilledHere 
            then [instr] -- Instruction doesn't use the spilled var, keep it.
            else error $ "RegisterAllocator: Unhandled instruction for spilled var: " ++ show instr ++ " Vars: " ++ show spilledHere