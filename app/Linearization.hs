-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Linearization where

import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import IR
import VMInterface (Register(..))
import PseudoInstructions
import Runtime

-- ===========================================================================
-- State
-- ===========================================================================

data LinState = LinState
    { prog :: ProgramIR 
    , code :: [Pseudo]  
    , lCount :: Int
    , vCount :: Integer
    }

newtype Lin a = Lin { runLin :: State LinState a }
    deriving (Functor, Applicative, Monad, MonadState LinState)

instance TempGen Lin where
    freshV = do
        c <- gets vCount
        modify $ \s -> s { vCount = c + 1 }
        return (VReg c)
    freshL prefix = do
        c <- gets lCount
        modify $ \s -> s { lCount = c + 1 }
        return $ prefix <> "_" <> T.pack (show c)

-- ===========================================================================
-- Main Logic
-- ===========================================================================

condScratch :: PReg
condScratch = Right RegH

-- | Topologically sort blocks (Reverse Post-Order)
-- Handles disjoint components (e.g., unreachable procedures) 
-- and ensures 'entry' is at the top.
topoSort :: ProgramIR -> [Block]
topoSort (ProgramIR m) = 
    let 
        lookupBlk l = Map.lookup l m

        getSuccs b = case blockTerm b of
            Jump l           -> [l]
            Branch _ _ _ t f -> [t, f] 
            _                -> []

        -- DFS Traversal
        go :: [Text] -> Set.Set Text -> [Block] -> (Set.Set Text, [Block])
        go [] seen acc = (seen, acc)
        go (lbl:lbls) seen acc
            | Set.member lbl seen = go lbls seen acc
            | otherwise = 
                case lookupBlk lbl of
                    Nothing -> go lbls seen acc 
                    Just blk -> 
                        let 
                            seen' = Set.insert lbl seen
                            succs = getSuccs blk
                            -- 1. Visit Children fully
                            (seenFinal, accFinal) = go succs seen' acc
                        in 
                            -- 2. Add Self to Accumulator (Pre-Order)
                            -- 3. Process remaining siblings
                            go lbls seenFinal (blk : accFinal)

        -- STRATEGY:
        -- The 'go' function builds the list like a stack: (Parent : Children : PreviousAcc).
        -- To get [Entry, Main, ProcA, ...], we must process the components in reverse desired order.
        -- 1. Process Procedures -> acc = [Procs]
        -- 2. Process Entry      -> acc = [Entry, Main, Procs]
        
        allLabels = Map.keys m
        entryLabel = "entry"
        otherLabels = filter (/= entryLabel) allLabels
        
        -- We put 'entry' LAST in the roots list so it is processed LAST,
        -- putting it at the HEAD of the final accumulator.
        roots = otherLabels ++ [entryLabel]

        (_, postOrder) = go roots Set.empty []
    in 
    postOrder

linearize :: ProgramIR -> [Pseudo]
linearize ir = 
    let 
        -- 1. Order blocks Topologically (Entry -> Main -> Loop)
        orderedBlocks = topoSort ir

        -- 2. Calculate Max VReg from reachable blocks
        maxV = foldr maxVReg 0 (concatMap blockInsns orderedBlocks)
        
        initialState = LinState { prog = ir, code = [], lCount = 0, vCount = maxV + 1 }
        finalState = execState (runLin $ mapM_ expandBlock orderedBlocks) initialState
    in removeRedundantJumps $ reverse (code finalState)

-- | Append Pseudo instruction
emit :: Pseudo -> Lin ()
emit p = modify $ \s -> s { code = p : code s }

-- | Append sequence
emitSeq :: [Pseudo] -> Lin ()
emitSeq = mapM_ emit

expandBlock :: Block -> Lin ()
expandBlock blk = do
    emit $ P_LABEL (blockLabel blk)
    mapM_ expandMiddle (blockInsns blk)
    expandTerminator (blockTerm blk)

-- ===========================================================================
-- Peephole Optimization: Remove Redundant Jumps
-- ===========================================================================

-- | Scan the code for "JUMP L1" immediately followed by "LABEL L1".
-- If found, remove the JUMP.
removeRedundantJumps :: [Pseudo] -> [Pseudo]
removeRedundantJumps [] = []
removeRedundantJumps (P_JUMP (T_Label l1) : next@(P_LABEL l2) : rest)
    | l1 == l2  = removeRedundantJumps (next : rest) -- Discard Jump, keep Label
    | otherwise = P_JUMP (T_Label l1) : removeRedundantJumps (next : rest)
removeRedundantJumps (x:xs) = x : removeRedundantJumps xs

-- ===========================================================================
-- Instruction Selection / Expansion
-- ===========================================================================

expandMiddle :: Middle -> Lin ()

-- Move: RST A; ADD src; SWP dest
expandMiddle (Move vDest op) = do
    emitMov (Left vDest) op

-- INC/DEC

expandMiddle (Compute OpAdd r1 (OpReg r2) (OpImm 1))
    | r1 == r2 = emit $ P_INC (Left r1)
    | otherwise = do
        emit $ P_RST (Right RegA)
        emitAdd (OpReg r2)
        emit $ P_INC (Right RegA)
        emit $ P_SWP (Left r1)

expandMiddle (Compute OpSub r1 (OpReg r2) (OpImm 1))
    | r1 == r2 = emit $ P_DEC (Left r1)
    | otherwise = do
        emit $ P_RST (Right RegA)
        emitAdd (OpReg r2)
        emit $ P_DEC (Right RegA)
        emit $ P_SWP (Left r1)

-- expandMiddle (Compute OpSub (OpReg r) (OpReg r) (OpImm 1)) = do
--     emit $ P_DEC (Left r)

-- Arithmetic
expandMiddle (Compute op vDest v1 v2) = do
    let pDest = Left vDest
    case op of
        OpAdd -> case (v1, v2) of

            -- Case: Both are Immediate
            (OpImm iVal1, OpImm iVal2) -> do
                 expandMiddle (Move vDest (OpImm (iVal1 + iVal2)))

            -- Case: v1 is Immediate
            (OpImm iVal, _) -> do
                 -- You can now use 'iVal'
                 genConst iVal (Right RegA)
                 emitAdd v2
                 emit $ P_SWP pDest
            
            -- Case: v2 is Immediate
            (_, OpImm iVal) -> do
                 -- You can now use 'iVal'
                 genConst iVal (Right RegA)
                 emitAdd v1
                 emit $ P_SWP pDest

            -- Case: Both are Registers
            (OpReg _, OpReg _) -> do
                 emit $ P_RST (Right RegA)
                 emitAdd v1
                 emitAdd v2
                 emit $ P_SWP pDest


        OpSub -> case (v1, v2) of
            -- Case: Both are Immediate
            (OpImm a, OpImm b) -> do
                 emitMov pDest (OpImm (max 0 (a - b)))

            (OpImm iVal, _) -> do
                 genConst iVal (Right RegA)
                 emitSub v2
                 emit $ P_SWP pDest
            
            (OpReg r, OpImm iVal) -> do
                 genConst iVal (Right RegA)

                 if r == vDest then do
                    emit $ P_SWP (Left r)  -- RegA = x, x = 13
                    emitSub (OpReg r)      -- RegA = x - 13
                    emit $ P_SWP pDest     -- x = result
                 else do
                    emit $ P_SWP pDest 
                    emit $ P_RST (Right RegA)
                    emitAdd v1 
                    emitSub (OpReg vDest)
                    emit $ P_SWP pDest

            -- Case: Both are Registers
            (OpReg _, OpReg _) -> do
                 emit $ P_RST (Right RegA)
                 emitAdd v1
                 emitSub v2
                 emit $ P_SWP pDest


        -- Templates
        OpMul -> case (v1, v2) of
            -- Case: Both are Immediate
            (OpImm a, OpImm b) -> do
                 emitMov pDest (OpImm (a * b))

            -- Case: v1 is Immediate
            (OpImm iVal, OpReg r) -> do
                 -- You can now use 'iVal'
                 genConstMul iVal (Left r)
                 emit $ P_SWP pDest
            
            -- Case: v2 is Immediate
            (OpReg r, OpImm iVal) -> do
                 -- You can now use 'iVal'
                 genConstMul iVal (Left r)
                 emit $ P_SWP pDest

            -- Case: Both are Registers
            (OpReg _, OpReg _) -> do
                (p1, p2) <- prepArgs v1 v2
                genMul pDest p1 p2
            -- emitSeq seq

        OpDiv -> case (v1,v2) of
            (OpReg r, OpImm 2) -> do
                -- Division by 2 optimization: SHR
                emit $ P_RST (Right RegA)
                emit $ P_ADD (Left r)
                emit $ P_SHR (Right RegA)
                emit $ P_SWP pDest
            (OpReg r, OpImm 4) -> do
                -- Division by 4 optimization: SHR
                emit $ P_RST (Right RegA)
                emit $ P_ADD (Left r)
                emit $ P_SHR (Right RegA)
                emit $ P_SHR (Right RegA)
                emit $ P_SWP pDest
            (OpReg r, OpImm 8) -> do
                -- Division by 8 optimization: SHR
                emit $ P_RST (Right RegA)
                emit $ P_ADD (Left r)
                emit $ P_SHR (Right RegA)
                emit $ P_SHR (Right RegA)
                emit $ P_SHR (Right RegA)
                emit $ P_SWP pDest
            _ -> do
                (p1, p2) <- prepArgs v1 v2
                (q, _) <- genDivMod p1 p2
                emitMov pDest (OpReg q)

        OpMod -> do --case (v1,v2) of
            -- (OpReg r, OpImm 2) -> do
            --     -- modulo by 2 optimization: SHR
            --     emit $ P_RST (Right RegA)
            --     emit $ P_ADD (Left r)
            --     emit $ P_SWP pDest
            --     emit $ P_RST (Right RegA)
            --     emit $ P_ADD (Left r)
            --     emit $ P_SHR pDest
            --     emit $ P_SHL pDest
            --     emit $ P_SUB pDest
            --     emit $ P_SWP pDest
            -- (_,_) -> do
            (p1, p2) <- prepArgs v1 v2
            (_, r) <- genDivMod p1 p2
            emitMov pDest (OpReg r)

-- Memory
expandMiddle (Load vDest addr) = do
    emit $ P_LOAD (T_Addr addr) -- A = Mem
    emit $ P_SWP (Left vDest)   -- vDest = A

expandMiddle (Store addr op) = do
    emit $ P_RST (Right RegA)
    emitAdd op                  -- A = op
    emit $ P_STORE (T_Addr addr)

expandMiddle (LoadIndirect vDest vPtr) = do
    -- We need P_RLOAD PReg.
    -- vPtr is VReg. P_RLOAD (Left vPtr) is valid abstractly.
    emit $ P_RLOAD (Left vPtr)  -- A = Mem[vPtr]
    emit $ P_SWP (Left vDest)   -- vDest = A

expandMiddle (StoreIndirect vPtr op) = do
    -- Mem[vPtr] <- op
    -- Need A = op.
    emit $ P_RST (Right RegA)
    emitAdd op                  -- A = op
    emit $ P_RSTORE (Left vPtr)

-- Call
expandMiddle (Call lbl args) = do
    ir <- gets prog
    let targetArgs = case Map.lookup lbl (case ir of ProgramIR m -> m) of
            Just b -> blockArgs b
            Nothing -> error $ "Call to undefined label: " ++ T.unpack lbl
    
    let pairs = zip args targetArgs
    
    forM_ pairs $ \(op, vTarget) -> do
        -- vTarget <- op
        -- Emit Move
        emitMov (Left vTarget) op
        
    emit $ P_CALL (T_Label lbl)

-- IO / Ret
expandMiddle (Print v) = do
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left v)
    emit P_WRITE

expandMiddle (ReadInput v) = do
    emit P_READ
    emit $ P_SWP (Left v)

expandMiddle (StoreRetAddr v) = do
    -- RegA has RetAddr.
    emit $ P_SWP (Left v)

expandMiddle (LoadRetAddr v) = do
    emit $ P_SWP (Left v) -- A = v

expandMiddle (Comment _) = return ()

-- ===========================================================================
-- Terminator Expansion
-- ===========================================================================

expandTerminator :: Terminator -> Lin ()
expandTerminator (Jump l) = emit $ P_JUMP (T_Label l)
expandTerminator (Return) = emit P_RTRN
expandTerminator Halt = emit P_HALT

expandTerminator (Branch cond v1 op2 t f) = do
    -- FIX 1: Always move the second operand into a temporary register 
    -- BEFORE starting the comparison. This prevents genConst from 
    -- wiping out RegA while v1 is already loaded there.
    p2 <- case op2 of
            OpReg r -> return (Left r)
            OpImm n -> do
                tmp <- freshV
                genConst n (Left tmp)
                return (Left tmp)

    case cond of
        -- v1 > p2  =>  v1 - p2 > 0
        Gt -> do
            emit $ P_RST (Right RegA)
            emit $ P_ADD (Left v1)
            emit $ P_SUB p2
            emit $ P_JPOS (T_Label t)
            emit $ P_JUMP (T_Label f)

        -- v1 < p2  =>  p2 - v1 > 0
        Lt -> do
            emit $ P_RST (Right RegA)
            emit $ P_ADD p2
            emit $ P_SUB (Left v1)
            emit $ P_JPOS (T_Label t)
            emit $ P_JUMP (T_Label f)

        -- v1 >= p2  =>  NOT (p2 > v1)
        Ge -> do
            emit $ P_RST (Right RegA)
            emit $ P_ADD p2
            emit $ P_SUB (Left v1)
            emit $ P_JPOS (T_Label f) -- If p2 > v1, it's False
            emit $ P_JUMP (T_Label t)

        -- v1 <= p2  =>  NOT (v1 > p2)
        Le -> do
            emit $ P_RST (Right RegA)
            emit $ P_ADD (Left v1)
            emit $ P_SUB p2
            emit $ P_JPOS (T_Label f) -- If v1 > p2, it's False
            emit $ P_JUMP (T_Label t)

        -- v1 == p2  =>  NOT (v1 > p2) AND NOT (p2 > v1)
        Eq -> do
            emit $ P_RST (Right RegA)
            emit $ P_ADD (Left v1)
            emit $ P_SUB p2
            emit $ P_JPOS (T_Label f) -- Difference > 0? Not equal.
            
            emit $ P_RST (Right RegA)
            emit $ P_ADD p2
            emit $ P_SUB (Left v1)
            emit $ P_JPOS (T_Label f) -- Rev Difference > 0? Not equal.
            
            emit $ P_JUMP (T_Label t) -- Both 0? Equal.

        -- v1 != p2  =>  (v1 > p2) OR (p2 > v1)
        Neq -> do
            emit $ P_RST (Right RegA)
            emit $ P_ADD (Left v1)
            emit $ P_SUB p2
            emit $ P_JPOS (T_Label t) -- Difference > 0? Unequal!
            
            emit $ P_RST (Right RegA)
            emit $ P_ADD p2
            emit $ P_SUB (Left v1)
            emit $ P_JPOS (T_Label t) -- Rev Difference > 0? Unequal!
            
            emit $ P_JUMP (T_Label f)







-- ===========================================================================
-- Helpers
-- ===========================================================================

-- | Generates code for: vDest <- op
emitMov :: PReg -> Operand -> Lin ()
emitMov pDest op = do
    case op of
        OpImm n -> genConst n pDest
        OpReg v -> do
            -- vDest <- v
            emit $ P_RST (Right RegA)
            emit $ P_ADD (Left v)
            emit $ P_SWP pDest

-- | Generates: RegA += op
emitAdd :: Operand -> Lin ()
emitAdd (OpReg v) = emit $ P_ADD (Left v)
emitAdd (OpImm n) = do
    -- We need to add Constant to RegA.
    -- Cannot use P_ADD (Imm). Must generate in scratch.
    t <- freshV
    genConst n (Left t)
    emit $ P_ADD (Left t)

-- | Generates: RegA -= op
emitSub :: Operand -> Lin ()
emitSub (OpReg v) = emit $ P_SUB (Left v)
emitSub (OpImm n) = do
    t <- freshV
    genConst n (Left t)
    emit $ P_SUB (Left t)

emitSubCond :: Operand -> Lin ()
emitSubCond (OpReg v) =
    emit $ P_SUB (Left v)
emitSubCond (OpImm n) = do
    genConst n condScratch
    emit $ P_SUB condScratch

-- | Prepare operands for templates (ensure they are VRegs/PRegs)
prepArgs :: Operand -> Operand -> Lin (PReg, PReg)
prepArgs o1 o2 = do
    r1 <- opToReg o1
    r2 <- opToReg o2
    return (r1, r2)
  where
    opToReg (OpReg v) = return (Left v)
    opToReg (OpImm n) = do
        v <- freshV
        genConst n (Left v)
        return (Left v)

-- -- | Generate Constant in-place (Placeholder)
-- genConst :: Integer -> PReg -> Lin ()
-- genConst n r = return () -- Placeholder as requested

-- generateConst :: Integer -> Register -> [VMCommand] -- Uses NAF representation to generate constants in lowest cost way, since NAF minimizes number of non zero digits, and 0s cost 1 and non zero costs 2. can be used for constant multiplication. generating constants using NAF is 12.5% faster than binary and NAF const multiplication is over 25% faster than binary
-- generateConst = go [] where
--     go :: [VMCommand] -> Integer -> Register -> [VMCommand]
--     go x 0 _ = x
--     go x y reg
--         | even y                     = go ((SHL reg) : x) (y `div` 2) reg
--         | (y `mod` 4) == 3 && y /= 3 = go ((DEC reg) : x) (y + 1) reg
--         | otherwise                  = go ((INC reg) : x) (y - 1) reg

-- | Generate Constant in-place using NAF optimization
-- Adapted from generateConst :: Integer -> Register -> [VMCommand]
-- genConst :: Integer -> PReg -> Lin ()
-- genConst 0 reg = emit $ P_RST reg
-- genConst n reg = do
--     emit $ P_RST reg -- Start from 0
    
--     let ops = generateNAF [] n reg
--     mapM_ emit ops

--   where
--     -- Helper strictly mirroring your logic, but returning [Pseudo]
--     generateNAF :: [Pseudo] -> Integer -> PReg -> [Pseudo]
--     generateNAF acc 0 _ = acc
--     generateNAF acc y r
--         | even y                     = generateNAF (P_SHL r : acc) (y `div` 2) r
--         | (y `mod` 4) == 3 && y /= 3 = generateNAF (P_DEC r : acc) (y + 1) r
--         | otherwise                  = generateNAF (P_INC r : acc) (y - 1) r

-- genConstMul :: Integer -> PReg -> Lin () -- A <- reg * n
-- genConstMul 0 reg = emit $ P_RST (Right RegA)
-- genConstMul n reg = do
--     emit $ P_RST (Right RegA) -- Start from 0
    
--     let ops = generateNAF [] n reg
--     mapM_ emit ops

--   where
--     -- Helper strictly mirroring your logic, but returning [Pseudo]
--     generateNAF :: [Pseudo] -> Integer -> PReg -> [Pseudo]
--     generateNAF acc 0 _ = acc
--     generateNAF acc y r
--         | even y                     = generateNAF (P_SHL (Right RegA) : acc) (y `div` 2) r
--         | (y `mod` 4) == 3 && y /= 3 = generateNAF (P_SUB r : acc) (y + 1) r
--         | otherwise                  = generateNAF (P_ADD r : acc) (y - 1) r

-- | Generate Constant safely in RegA, then move to destination
genConst :: Integer -> PReg -> Lin ()
genConst n pDest = do
    emit $ P_RST (Right RegA) -- Build in HW Acc
    let ops = generateNAF [] n (Right RegA)
    mapM_ emit ops
    -- Only move to destination once the number is complete
    if pDest /= Right RegA 
        then emit $ P_SWP pDest
        else return ()
  where
    generateNAF acc 0 _ = acc
    generateNAF acc y r
        | even y                     = generateNAF (P_SHL r : acc) (y `div` 2) r
        | (y `mod` 4) == 3 && y /= 3 = generateNAF (P_DEC r : acc) (y + 1) r
        | otherwise                  = generateNAF (P_INC r : acc) (y - 1) r

-- | Generate Multiplication by Constant (A = reg * n)
genConstMul :: Integer -> PReg -> Lin ()
genConstMul n pSrc = do
    -- If we are multiplying by a constant, we use RegA as the accumulator.
    -- We must not clobber the pSrc if it's in RegA, so we move it to a scratch first if needed.
    case pSrc of
        Right RegA -> do
            t <- freshV
            emit $ P_SWP (Left t)
            genConstMul n (Left t)
        _ -> do
            emit $ P_RST (Right RegA)
            let ops = generateNAF [] n pSrc
            mapM_ emit ops
  where
    generateNAF acc 0 _ = acc
    generateNAF acc y r
        | even y                     = generateNAF (P_SHL (Right RegA) : acc) (y `div` 2) r
        | (y `mod` 4) == 3 && y /= 3 = generateNAF (P_SUB r : acc) (y + 1) r
        | otherwise                  = generateNAF (P_ADD r : acc) (y - 1) r


maxVReg :: Middle -> Integer -> Integer
maxVReg instr m = case instr of
    Move (VReg v) _ -> max v m
    Compute _ (VReg v) _ _ -> max v m
    Load (VReg v) _ -> max v m
    _ -> m


-- ===========================================================================
-- Software Math Implementations
-- ===========================================================================

-- | Generate Multiplication (vDest = v1 * v2)
-- Algorithm: Russian Peasant (Shift-and-Add)
genMul :: PReg -> PReg -> PReg -> Lin ()
genMul pDest p1 p2 = do
    -- We need fresh registers for the mutable copies of operands and result
    val1 <- freshV -- Left Operand (will be shifted Left)
    val2 <- freshV -- Right Operand (will be shifted Right)
    res  <- freshV -- Accumulator
    
    -- Labels
    loopLbl <- freshL "mul_loop"
    endLbl  <- freshL "mul_end"
    skipLbl <- freshL "mul_skip"

    -- Initialize
    emitMov (Left val1) (opFromPReg p1)
    emitMov (Left val2) (opFromPReg p2)
    genConst 0 (Left res)

    emit $ P_LABEL loopLbl

    -- Check if val2 == 0. If so, we are done.
    -- (We optimize by checking if we can stop)
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left val2)
    emit $ P_JZERO (T_Label endLbl)

    -- Check Parity of val2 (is val2 Odd?)
    -- logic: parity = val2 - ((val2 >> 1) << 1)
    t <- freshV
    emitMov (Left t) (OpReg val2)
    emit $ P_SHR (Left t)
    emit $ P_SHL (Left t)
    
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left val2)
    emit $ P_SUB (Left t) 
    -- If A > 0 (meaning 1), it's odd.
    emit $ P_JZERO (T_Label skipLbl)

    -- It is Odd: Add val1 to Result
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left res)
    emit $ P_ADD (Left val1)
    emit $ P_SWP (Left res)

    emit $ P_LABEL skipLbl
    
    -- Shift val1 Left (Double it)
    emit $ P_SHL (Left val1)
    
    -- Shift val2 Right (Halve it)
    emit $ P_SHR (Left val2)
    
    emit $ P_JUMP (T_Label loopLbl)

    emit $ P_LABEL endLbl
    
    -- Move Result to Dest
    emitMov pDest (OpReg res)

-- | Generate Division and Modulo
-- Returns (Quotient, Remainder)
-- Algorithm: Binary Long Division
genDivMod :: PReg -> PReg -> Lin (VReg, VReg)
genDivMod pN pD = do
    n <- freshV -- Numerator (Remains)
    d <- freshV -- Denominator (Shifted)
    q <- freshV -- Quotient
    bit <- freshV -- Bit mask (1, 2, 4...)

    loopShift <- freshL "div_shift"
    loopSub   <- freshL "div_sub"
    endLbl    <- freshL "div_end"
    skipSub   <- freshL "div_skip_sub"
    endZero   <- freshL "end_zero"

    -- Init
    emitMov (Left n) (opFromPReg pN)
    emitMov (Left d) (opFromPReg pD)
    genConst 0 (Left q)
    genConst 1 (Left bit)

    -- Check for divide by zero (Optional, skipping for speed)
    emit $ P_SWP (Left d)
    emit $ P_JZERO (T_Label endZero)
    emit $ P_SWP (Left d)
    
    -- Phase 1: Shift D left until it's greater than N
    -- (Or until we are about to overflow, practically 63 bits)
    emit $ P_LABEL loopShift
    
    -- if D >= n (actually checks if n < D), stop shifting
    -- Check: n - D < 0
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left n)
    emit $ P_SUB (Left d) 
    -- If result is negative (or zero? JPOS is >0), wait.
    -- Strict check: if D > N, we stop. 
    -- Implementation: if n - D < 0.
    -- Since we don't have JNEG, we use JPOS on (D - N). 
    -- If (D - N) > 0, stop.
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left d)
    emit $ P_SUB (Left n)
    emit $ P_JPOS (T_Label loopSub)
    
    -- Check for overflow (if D becomes huge). 
    -- Simple heuristic: if D < 0 (high bit set), stop. 
    -- Assuming signed ints? Let's assume positive inputs for now.
    
    emit $ P_SHL (Left d)
    emit $ P_SHL (Left bit)
    emit $ P_JUMP (T_Label loopShift)

    -- Phase 2: Shift Right and Subtract
    emit $ P_LABEL loopSub

    -- if bit == 0, end
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left bit)
    emit $ P_JZERO (T_Label endLbl)

    -- if n >= D
    -- Check: n - D >= 0
    -- JPOS checks > 0. JZERO checks == 0.
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left n)
    emit $ P_SUB (Left d)
    
    -- If A < 0 (N < D), skip subtraction.
    -- We don't have JNEG. We check if D > N.
    -- Re-calc: D - N
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left d)
    emit $ P_SUB (Left n)
    -- If D > n (strictly positive), skip
    emit $ P_JPOS (T_Label skipSub)

    -- Subtract: n = n - D
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left n)
    emit $ P_SUB (Left d)
    emit $ P_SWP (Left n)

    -- Add bit to Q
    emit $ P_RST (Right RegA)
    emit $ P_ADD (Left q)
    emit $ P_ADD (Left bit)
    emit $ P_SWP (Left q)

    emit $ P_LABEL skipSub
    
    emit $ P_SHR (Left d)
    emit $ P_SHR (Left bit)
    emit $ P_JUMP (T_Label loopSub)
    emit $ P_LABEL endZero
    emit $ P_RST (Left n)
    emit $ P_RST (Left q)

    emit $ P_LABEL endLbl
    
    return (q, n)

-- Helper to convert PReg back to Operand for move
opFromPReg :: PReg -> Operand
opFromPReg (Left v) = OpReg v
opFromPReg (Right r) = error "Cannot generate operand from PhysReg directly"