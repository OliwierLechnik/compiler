{-# LANGUAGE OverloadedStrings #-}

module RegisterNaiveAllocator (allocate) where

import PseudoInstructions
import VMInterface (Register(..), Address)
import IR (VReg(..))
import Data.List (nub)
import qualified Data.Map as Map
import Data.Map (Map)

-- ============================================================================
-- Configuration
-- ============================================================================

-- | The scratch register used for shuttling data between memory and RegA.
-- We assume RegH is volatile and can be clobbered at any time.
scratchReg :: Register
scratchReg = RegH

-- | We start allocating memory from the top of the address space downwards.
-- Ensure this does not overlap with your program code or heap.
spillBaseAddr :: Address
spillBaseAddr = (2^62) - 1

-- ============================================================================
-- Allocator Main
-- ============================================================================

allocate :: [Pseudo] -> [Pseudo]
allocate instrs = 
    let 
        -- 1. Identify all unique Virtual Registers used in the program
        allVRegs = nub $ concatMap extractVRegs instrs
        
        -- 2. Assign a static memory address to every VReg
        -- Map: VReg -> Address
        addrMap = Map.fromList $ zip allVRegs [spillBaseAddr, spillBaseAddr - 1 ..]
    in 
    -- 3. Rewrite the instruction stream
    concatMap (rewriteInstr addrMap) instrs

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Extract all VRegs mentioned in an instruction (operands or targets)
extractVRegs :: Pseudo -> [VReg]
extractVRegs p = case p of
    P_RLOAD r   -> checkReg r
    P_RSTORE r  -> checkReg r
    P_ADD r     -> checkReg r
    P_SUB r     -> checkReg r
    P_SWP r     -> checkReg r
    P_RST r     -> checkReg r
    P_INC r     -> checkReg r
    P_DEC r     -> checkReg r
    P_SHL r     -> checkReg r
    P_SHR r     -> checkReg r
    P_LOAD t    -> checkTgt t
    P_STORE t   -> checkTgt t
    P_JUMP t    -> checkTgt t
    P_JPOS t    -> checkTgt t
    P_JZERO t   -> checkTgt t
    P_CALL t    -> checkTgt t
    _           -> []
  where
    checkReg (Left v) = [v]
    checkReg _        = []
    checkTgt (T_VReg v) = [v]
    checkTgt _          = []

-- | Look up the memory address for a VReg. Crashes if VReg not found (should be impossible).
getAddr :: Map VReg Address -> VReg -> Address
getAddr mapping v = case Map.lookup v mapping of
    Just addr -> addr
    Nothing   -> error $ "SimpleAllocator: VReg " ++ show v ++ " has no assigned address."

-- ============================================================================
-- Rewriting Logic (The "Spill Everything" Expansion)
-- ============================================================================

rewriteInstr :: Map VReg Address -> Pseudo -> [Pseudo]
rewriteInstr mapping instr = case instr of

    -- 1. DIRECT LOAD: RegA <- v
    -- P_LOAD (T_VReg v) becomes P_LOAD (T_Addr a)
    P_LOAD (T_VReg v) -> 
        [ P_LOAD (T_Addr (getAddr mapping v)) ]

    -- 2. DIRECT STORE: v <- RegA
    P_STORE (T_VReg v) -> 
        [ P_STORE (T_Addr (getAddr mapping v)) ]

    -- 3. BINARY ARITHMETIC: RegA = RegA op v
    -- Strategy: Save A to Scratch, Load v to A, Swap (A=OldA, Scratch=v), Op Scratch
    P_ADD (Left v) -> expandBinary (getAddr mapping v) P_ADD
    P_SUB (Left v) -> expandBinary (getAddr mapping v) P_SUB

    -- 4. UNARY READ-MODIFY-WRITE: v = op(v)
    -- Strategy: Save A, Load v, Op A, Store v, Restore A
    P_INC (Left v) -> expandUnary (getAddr mapping v) P_INC
    P_DEC (Left v) -> expandUnary (getAddr mapping v) P_DEC
    P_SHL (Left v) -> expandUnary (getAddr mapping v) P_SHL
    P_SHR (Left v) -> expandUnary (getAddr mapping v) P_SHR

    -- 5. RESET: v = 0
    -- Strategy: Save A, Reset A, Store v, Restore A
    P_RST (Left v) ->
        [ P_SWP (Right scratchReg)          -- Save A
        , P_RST (Right RegA)                -- A = 0
        , P_STORE (T_Addr (getAddr mapping v)) -- Mem[v] = 0
        , P_SWP (Right scratchReg)          -- Restore A
        ]

    -- 6. SWAP: RegA <-> v
    P_SWP (Left v) ->
        let addr = getAddr mapping v in
        [ P_SWP (Right scratchReg)          -- H = NewVal (was A), A = OldH (garbage)
        , P_LOAD (T_Addr addr)              -- A = OldVal (from Mem[v])
        , P_SWP (Right scratchReg)          -- A = NewVal, H = OldVal
        , P_STORE (T_Addr addr)             -- Mem[v] = NewVal
        , P_SWP (Right scratchReg)          -- A = OldVal, H = NewVal
        ]

    -- 7. INDIRECT LOAD: RegA <- Mem[v]
    -- v acts as a pointer.
    P_RLOAD (Left v) ->
        [ P_LOAD (T_Addr (getAddr mapping v)) -- A = Pointer (value of v)
        , P_SWP (Right scratchReg)            -- H = Pointer
        , P_RLOAD (Right scratchReg)          -- A = Mem[H]
        ]

    -- 8. INDIRECT STORE: Mem[v] <- RegA
    P_RSTORE (Left v) ->
        [ P_SWP (Right scratchReg)            -- H = Value to Store (was A)
        , P_LOAD (T_Addr (getAddr mapping v)) -- A = Pointer (value of v)
        , P_SWP (Right scratchReg)            -- A = Value, H = Pointer
        , P_RSTORE (Right scratchReg)         -- Mem[H] = A
        ]

    -- 9. CONTROL FLOW TARGETS
    -- Map T_VReg to T_Addr. Assumes VM supports JUMP to absolute address.
    P_JUMP (T_VReg v)   -> [P_JUMP (T_Addr (getAddr mapping v))]
    P_JPOS (T_VReg v)   -> [P_JPOS (T_Addr (getAddr mapping v))]
    P_JZERO (T_VReg v)  -> [P_JZERO (T_Addr (getAddr mapping v))]
    P_CALL (T_VReg v)   -> [P_CALL (T_Addr (getAddr mapping v))]

    -- 10. Pass-through for Physical Registers and non-VReg Targets
    -- Even if the instruction uses physical regs, we keep it as is.
    other -> [other]

  where
    -- Helper for A = A op Mem[v]
    expandBinary addr opConstructor =
        [ P_SWP (Right scratchReg)      -- Save A to H
        , P_LOAD (T_Addr addr)          -- Load v to A
        , P_SWP (Right scratchReg)      -- A = OldA, H = v
        , opConstructor (Right scratchReg) -- A = A op H
        ]

    -- Helper for Mem[v] = op(Mem[v]) (preserving A)
    expandUnary addr opConstructor =
        [ P_SWP (Right scratchReg)      -- Save A to H
        , P_LOAD (T_Addr addr)          -- Load v to A
        , opConstructor (Right RegA)    -- Perform Op on A
        , P_STORE (T_Addr addr)         -- Store result back to v
        , P_SWP (Right scratchReg)      -- Restore A from H
        ]