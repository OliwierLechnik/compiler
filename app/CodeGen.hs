-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module CodeGen where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, nub)
import Control.Monad (forM, forM_, filterM)

import AST
import IR
import qualified VMInterface as VM

-- ===========================================================================
-- Types and State
-- ===========================================================================

data VarLoc
    = LocReg              
    | LocArray VM.Address Integer 
    | LocRef   VReg 
    | LocArrayRef VReg VReg -- FATPTR -- (Base Pointer VReg, Start Index VReg)
    deriving (Show, Eq)

type SymTable = Map Pid VarLoc
type VarEnv = Map Pid VReg

data CodegenState = CodegenState
    { currentBlockInsns :: [Middle]      
    , currentBlockPhis  :: [Phi]         
    , currentBlockArgs  :: [VReg]        
    , finishedBlocks    :: Map Label Block
    , currentLabel      :: Label         
    , labelCount        :: Int           
    , vregCount         :: Integer       
    , freeMem           :: VM.Address    
    , symTable          :: SymTable
    , varEnv            :: VarEnv
    }

type Codegen a = State CodegenState a

initialState :: CodegenState
initialState = CodegenState
    { currentBlockInsns = []
    , currentBlockPhis  = []
    , currentBlockArgs  = [] 
    , finishedBlocks    = Map.empty
    , currentLabel      = "entry"
    , labelCount        = 0
    , vregCount         = 0
    , freeMem           = 0
    , symTable          = Map.empty
    , varEnv            = Map.empty
    }

freshLabel :: Text -> Codegen Label
freshLabel prefix = do
    cnt <- gets labelCount
    modify $ \s -> s { labelCount = cnt + 1 }
    return $ prefix <> "_" <> T.pack (show cnt)

freshVReg :: Codegen VReg
freshVReg = do
    cnt <- gets vregCount
    modify $ \s -> s { vregCount = cnt + 1 }
    return $ VReg cnt

allocMem :: Integer -> Codegen VM.Address
allocMem size = do
    addr <- gets freeMem
    modify $ \s -> s { freeMem = addr + size }
    return addr

emit :: Middle -> Codegen ()
emit instr = modify $ \s -> s { currentBlockInsns = instr : currentBlockInsns s }

emitPhi :: Phi -> Codegen ()
emitPhi phi = modify $ \s -> s { currentBlockPhis = phi : currentBlockPhis s }

terminate :: Terminator -> Label -> Codegen ()
terminate term nextLabel = do
    s <- get
    let blk = Block 
            { blockLabel = currentLabel s
            , blockArgs  = currentBlockArgs s 
            , blockPhis  = reverse (currentBlockPhis s) 
            , blockInsns = reverse (currentBlockInsns s)
            , blockTerm  = term 
            }
    modify $ \st -> st 
        { finishedBlocks = Map.insert (blockLabel blk) blk (finishedBlocks st)
        , currentLabel   = nextLabel
        , currentBlockInsns = [] 
        , currentBlockPhis  = [] 
        , currentBlockArgs  = [] 
        }

startBlock :: Label -> Codegen ()
startBlock lbl = modify $ \s -> s 
    { currentLabel = lbl
    , currentBlockInsns = []
    , currentBlockPhis = [] 
    , currentBlockArgs = [] 
    }

updateBlock :: Label -> (Block -> Block) -> Codegen ()
updateBlock lbl f = modify $ \s -> 
    s { finishedBlocks = Map.adjust f lbl (finishedBlocks s) }

addVar :: Pid -> VarLoc -> Codegen ()
addVar name loc = modify $ \s -> s { symTable = Map.insert name loc (symTable s) }

checkVar :: Pid -> Codegen Bool
checkVar pid = do
    syms <- gets symTable
    return $ Map.member pid syms

writeVar :: Pid -> VReg -> Codegen ()
writeVar pid val = do
    exists <- checkVar pid
    if not exists then do
        addVar pid LocReg
        modify $ \s -> s { varEnv = Map.insert pid val (varEnv s) }
    else do
        loc <- getVarLoc pid
        case loc of
            LocReg -> modify $ \s -> s { varEnv = Map.insert pid val (varEnv s) }
            LocRef ptrReg -> do 
                emit $ StoreIndirect ptrReg (OpReg val)
            LocArrayRef baseReg startReg -> error $ "Cannot write to whole array " ++ show pid ++ " as scalar"
            LocArray _ _ -> error $ "Cannot write to array " ++ show pid

readVar :: Pid -> Codegen Operand
readVar pid = do
    loc <- getVarLoc pid
    case loc of
        LocReg -> do
            env <- gets varEnv
            case Map.lookup pid env of
                Just r -> return $ OpReg r
                Nothing -> return $ OpImm 0
        LocRef ptrReg -> do 
            valReg <- freshVReg
            emit $ LoadIndirect valReg ptrReg
            return $ OpReg valReg
        LocArrayRef _ _ -> error "Cannot read array as scalar"
        LocArray _ _ -> error "Cannot read array as scalar"

getVarLoc :: Pid -> Codegen VarLoc
getVarLoc name = do
    syms <- gets symTable
    case Map.lookup name syms of
        Just loc -> return loc
        Nothing  -> error $ "Undefined variable: " ++ show name

onlySSA :: [Pid] -> Codegen [Pid]
onlySSA pids = filterM isLocReg pids
  where
    isLocReg pid = do
        syms <- gets symTable
        case Map.lookup pid syms of
            Just LocReg -> return True
            _           -> return False

getEnvVal :: Pid -> VarEnv -> Codegen Operand
getEnvVal pid env = case Map.lookup pid env of
    Just r -> return $ OpReg r
    Nothing -> return $ OpImm 0

-- ===========================================================================
-- Code Generation
-- ===========================================================================

genProgram :: ProgramAll -> ProgramIR
genProgram (ProgramAll procs mainBlock) = 
    let finalState = execState (genAll procs mainBlock) initialState
        lastBlk = Block 
            { blockLabel = currentLabel finalState
            , blockArgs  = currentBlockArgs finalState 
            , blockPhis  = reverse (currentBlockPhis finalState)
            , blockInsns = reverse (currentBlockInsns finalState)
            , blockTerm  = Halt 
            }
        allBlocks = Map.insert (blockLabel lastBlk) lastBlk (finishedBlocks finalState)
    in ProgramIR allBlocks

genAll :: [Procedure] -> Main -> Codegen ()
genAll procs (Main decls cmds) = do
    mainLabel <- freshLabel "main"
    terminate (Jump mainLabel) "temp_ignore_me"
    
    forM_ procs genProcedure
    
    startBlock mainLabel
    modify $ \s -> s { varEnv = Map.empty }
    forM_ decls genDeclaration
    forM_ cmds genCommand

genProcedure :: Procedure -> Codegen ()
genProcedure (Procedure _ pid args decls cmds) = do
    oldSyms <- gets symTable
    oldEnv  <- gets varEnv
    modify $ \s -> s { varEnv = Map.empty }
    
    let procLabel = "proc_" <> pid
    startBlock procLabel 

    retAddrReg <- freshVReg
    emit $ StoreRetAddr retAddrReg 
    
    -- FATPTR -- Updated Argument Handling
    argRegs <- forM args $ \(mType, argName) -> do
        case mType of
            Just Ttype -> do
                -- Array: Consumes 2 VRegs (Base, Start)
                baseReg <- freshVReg
                startReg <- freshVReg
                addVar argName (LocArrayRef baseReg startReg)
                return [baseReg, startReg]
            _ -> do
                -- Scalar: Consumes 1 VReg (Pointer)
                argPtrReg <- freshVReg
                addVar argName (LocRef argPtrReg)
                return [argPtrReg]
    
    -- Record flattened list of args for block header
    modify $ \s -> s { currentBlockArgs = concat argRegs }

    let showVReg (VReg n) = "v" ++ show n
    let argStr = intercalate ", " (map showVReg (concat argRegs))
    emit $ Comment $ "Args: " <> T.pack argStr

    forM_ decls genDeclaration
    forM_ cmds genCommand
    
    emit $ LoadRetAddr retAddrReg
    terminate Return "unreachable"
    
    modify $ \s -> s { symTable = oldSyms, varEnv = oldEnv }

genDeclaration :: Declaration -> Codegen ()
genDeclaration (DeclScalar _ pid) = addVar pid LocReg
genDeclaration (DeclArray _ pid start end) = do
    let size = end - start + 1
    addr <- allocMem size
    addVar pid (LocArray addr start)

genExpr :: Expr -> Codegen Operand
genExpr (Val v) = genValue v
genExpr (Add v1 v2) = genBinOp OpAdd v1 v2
genExpr (Sub v1 v2) = genBinOp OpSub v1 v2
genExpr (Mul v1 v2) = genBinOp OpMul v1 v2
genExpr (Div v1 v2) = genBinOp OpDiv v1 v2
genExpr (Mod v1 v2) = genBinOp OpMod v1 v2

genValue :: Value -> Codegen Operand
genValue (ValNum n) = return $ OpImm n
genValue (ValId ident) = case ident of
    Scalar _ pid -> readVar pid
    _ -> do
        res <- freshVReg
        (addrOp, isInd) <- genAddr ident
        case (addrOp, isInd) of
            (OpImm addr, False) -> emit $ Load res (fromIntegral addr)
            (OpReg reg, True)   -> emit $ LoadIndirect res reg
            _ -> error "Invalid addr"
        return $ OpReg res

genBinOp :: BinOp -> Value -> Value -> Codegen Operand
genBinOp op v1 v2 = do
    op1 <- genValue v1
    op2 <- genValue v2
    res <- freshVReg
    emit $ Compute op res op1 op2
    return $ OpReg res

genAddr :: Id -> Codegen (Operand, Bool)
genAddr (Scalar _ pid) = do
    loc <- getVarLoc pid
    case loc of
        LocRef ptrReg -> return (OpReg ptrReg, True)
        LocReg -> error "Internal error: genAddr called on Register Scalar"
        LocArray addr _ -> return (OpImm (fromIntegral addr), False) 
        LocArrayRef _ _ -> error "Internal error: Used array as scalar"

genAddr (ArrayConst _ pid idx) = do
    loc <- getVarLoc pid
    case loc of
        LocArray base start -> do
            let offset = idx - start
            return (OpImm (fromIntegral (base + offset)), False)
        LocRef ptrReg -> error "Cannot access LocRef as array (Type mismatch)"
        LocArrayRef baseReg startReg -> do -- FATPTR -- Const index on passed array
             finalAddr <- freshVReg
             -- offset = idx - startReg
             offReg <- freshVReg
             emit $ Compute OpSub offReg (OpImm idx) (OpReg startReg)
             -- addr = base + offset
             emit $ Compute OpAdd finalAddr (OpReg baseReg) (OpReg offReg)
             return (OpReg finalAddr, True)
        _ -> error "Scalar/Array mismatch"

genAddr (ArrayVar _ pid idxPid) = do
    idxVal <- genValue (ValId (Scalar (error "pos") idxPid))
    loc <- getVarLoc pid
    case loc of
        LocArray base start -> do
            finalReg <- freshVReg
            r1 <- freshVReg
            emit $ Compute OpSub r1 idxVal (OpImm start)
            emit $ Compute OpAdd finalReg (OpReg r1) (OpImm (fromIntegral base))
            return (OpReg finalReg, True)
        LocArrayRef baseReg startReg -> do -- FATPTR -- Dynamic index on passed array
            finalReg <- freshVReg
            r1 <- freshVReg
            -- r1 = idx - start
            emit $ Compute OpSub r1 idxVal (OpReg startReg)
            -- final = base + r1
            emit $ Compute OpAdd finalReg (OpReg baseReg) (OpReg r1)
            return (OpReg finalReg, True)
        LocRef ptrReg -> error "Used scalar ref as array"
        _ -> error "Type mismatch"

genCommand :: Command -> Codegen ()

genCommand (Assignment _ target expr) = do
    srcOp <- genExpr expr
    valReg <- case srcOp of { OpReg r -> return r; OpImm i -> do { r <- freshVReg; emit $ Move r (OpImm i); return r } }
    case target of
        Scalar _ pid -> writeVar pid valReg
        _ -> do
            (dstOp, isInd) <- genAddr target
            case (dstOp, isInd) of
                (OpImm addr, False) -> emit $ Store (fromIntegral addr) (OpReg valReg)
                (OpReg reg, True)   -> emit $ StoreIndirect reg (OpReg valReg)
                _ -> error "Store fail"

genCommand (MyRead _ target) = do
    res <- freshVReg
    emit $ ReadInput res
    case target of
        Scalar _ pid -> writeVar pid res
        _ -> do
            (dst, isInd) <- genAddr target
            case (dst, isInd) of
                (OpImm a, False) -> emit $ Store (fromIntegral a) (OpReg res)
                (OpReg r, True)  -> emit $ StoreIndirect r (OpReg res)
                _ -> error "Read error"

genCommand (MyWrite _ val) = do
    op <- genValue val
    r <- case op of { OpReg reg -> return reg; OpImm imm -> do { t <- freshVReg; emit $ Move t (OpImm imm); return t } }
    emit $ Print r

genCommand (ProcCall _ name args) = do
    let label = "proc_" <> name
    
    -- FATPTR -- Push 2 operands for Arrays, 1 for Scalars
    -- Result is a flattened list of operands [base1, start1, ptr2, ...]
    -- And a copyBack list for scalars.
    
    processedArgs <- forM args $ \argPid -> do
        loc <- getVarLoc argPid
        case loc of
            LocReg -> do
                -- Scalar Value -> Spill -> Pass Ptr
                val <- readVar argPid
                tmpAddr <- allocMem 1
                case val of
                    OpReg r -> emit $ Store tmpAddr (OpReg r)
                    OpImm i -> emit $ Store tmpAddr (OpImm i)
                return ([OpImm (fromIntegral tmpAddr)], Just (argPid, tmpAddr))
                
            LocArray base start -> do
                -- Array Global -> Pass Base, Pass Start
                return ([OpImm (fromIntegral base), OpImm start], Nothing)
                
            LocRef ptrReg -> do
                -- Scalar Ref -> Pass Ptr
                return ([OpReg ptrReg], Nothing)
                
            LocArrayRef baseReg startReg -> do
                -- Array Ref -> Pass Base, Pass Start
                return ([OpReg baseReg, OpReg startReg], Nothing)

    let operands = concatMap fst processedArgs
    let copyBacks = map snd processedArgs

    emit $ Call label operands

    forM_ copyBacks $ \cb -> case cb of
        Just (pid, addr) -> do
            newVal <- freshVReg
            emit $ Load newVal addr
            writeVar pid newVal
        Nothing -> return ()

-- ... (Control Flow Unchanged) ...

genCommand (If _ cond cmds) = do
    entryLabel <- gets currentLabel
    entryEnv   <- gets varEnv
    trueLabel <- freshLabel "if_true"
    endLabel  <- freshLabel "if_end"
    genCondition cond trueLabel endLabel
    forM_ cmds genCommand
    trueEndLabel <- gets currentLabel
    trueEndEnv   <- gets varEnv
    terminate (Jump endLabel) endLabel
    mergeEnvironments trueEndLabel trueEndEnv entryLabel entryEnv

genCommand (IfElse _ cond cmdsTrue cmdsFalse) = do
    entryLabel <- gets currentLabel
    entryEnv   <- gets varEnv
    trueLabel  <- freshLabel "if_true"
    falseLabel <- freshLabel "if_false"
    endLabel   <- freshLabel "if_end"
    genCondition cond trueLabel falseLabel
    forM_ cmdsTrue genCommand
    trueEndLabel <- gets currentLabel
    trueEndEnv   <- gets varEnv
    terminate (Jump endLabel) falseLabel
    modify $ \s -> s { varEnv = entryEnv } 
    forM_ cmdsFalse genCommand
    falseEndLabel <- gets currentLabel
    falseEndEnv   <- gets varEnv
    terminate (Jump endLabel) endLabel
    mergeEnvironments trueEndLabel trueEndEnv falseEndLabel falseEndEnv

genCommand (While _ cond cmds) = do
    entryLabel <- gets currentLabel
    entryEnv   <- gets varEnv
    condLabel <- freshLabel "while_cond"
    bodyLabel <- freshLabel "while_body"
    endLabel  <- freshLabel "while_end"
    
    terminate (Jump condLabel) condLabel
    
    -- 1. Setup Phi Nodes
    let modVars = scanModified cmds
    ssaVars <- onlySSA modVars
    loopPhis <- forM ssaVars $ \pid -> do
        valEntry <- getEnvVal pid entryEnv
        valPhi <- freshVReg
        emitPhi $ Phi valPhi [(entryLabel, valEntry)]
        writeVar pid valPhi
        return (pid, valPhi)

    -- 2. CRITICAL: Capture the Env state at the Loop Header
    -- This env maps 'a' -> 'vPhiA', 'b' -> 'vPhiB'
    loopHeaderEnv <- gets varEnv 

    genCondition cond bodyLabel endLabel
    forM_ cmds genCommand
    
    bodyEndLabel <- gets currentLabel
    bodyEndEnv   <- gets varEnv
    terminate (Jump condLabel) endLabel
    
    -- 3. Backpatching
    forM_ loopPhis $ \(pid, valPhi) -> do
        valBack <- getEnvVal pid bodyEndEnv
        updateBlock condLabel $ \blk ->
            let updatePhi (Phi r ops) = 
                    if r == valPhi 
                    then Phi r (ops ++ [(bodyEndLabel, valBack)]) 
                    else Phi r ops
            in blk { blockPhis = map updatePhi (blockPhis blk) }

    -- 4. FIX: Restore the Header Env for subsequent code
    modify $ \s -> s { varEnv = loopHeaderEnv }

genCommand (Repeat _ cmds cond) = do
    entryLabel <- gets currentLabel
    entryEnv   <- gets varEnv
    startLabel <- freshLabel "repeat_start"
    endLabel   <- freshLabel "repeat_end"
    terminate (Jump startLabel) startLabel
    let modVars = scanModified cmds
    ssaVars <- onlySSA modVars
    loopPhis <- forM ssaVars $ \pid -> do
        valEntry <- getEnvVal pid entryEnv
        valPhi <- freshVReg
        emitPhi $ Phi valPhi [(entryLabel, valEntry)]
        writeVar pid valPhi
        return (pid, valPhi)
    forM_ cmds genCommand
    bodyEndLabel <- gets currentLabel
    bodyEndEnv   <- gets varEnv
    genCondition cond endLabel startLabel
    forM_ loopPhis $ \(pid, valPhi) -> do
        valBack <- getEnvVal pid bodyEndEnv
        updateBlock startLabel $ \blk ->
            let updatePhi (Phi r ops) = 
                    if r == valPhi 
                    then Phi r (ops ++ [(bodyEndLabel, valBack)]) 
                    else Phi r ops
            in blk { blockPhis = map updatePhi (blockPhis blk) }

-- In CodeGen.hs
-- In CodeGen.hs

genCommand (ForLoop _ var startVal endVal dir cmds) = do
    -- 1. [PRE-HEADER] Setup Start Value
    startOp <- genValue startVal
    startReg <- freshVReg
    emit $ Move startReg startOp
    writeVar var startReg
    
    -- 2. [PRE-HEADER] Setup End Value (Invariant)
    rawEndOp <- genValue endVal
    endReg <- freshVReg
    emit $ Move endReg rawEndOp
    
    -- 3. Prepare Loop Entry
    entryLabel <- gets currentLabel
    entryEnv   <- gets varEnv
    loopLabel <- freshLabel "for_loop"
    endLabel  <- freshLabel "for_end"
    
    terminate (Jump loopLabel) loopLabel
    
    -- 4. [LOOP HEADER] Phi Nodes
    -- Note: 'var' (loop counter) is always modified, so we include it.
    let modVars = nub (var : scanModified cmds)
    ssaVars <- onlySSA modVars
    loopPhis <- forM ssaVars $ \pid -> do
        valEntry <- getEnvVal pid entryEnv
        valPhi <- freshVReg
        emitPhi $ Phi valPhi [(entryLabel, valEntry)]
        writeVar pid valPhi
        return (pid, valPhi)
        
    -- 5. CRITICAL: Capture the Env state at the Loop Header
    -- This ensures code after the loop uses the Phi values (correct values at exit).
    loopHeaderEnv <- gets varEnv

    -- 6. [LOOP HEADER] Condition Check
    -- Check if we should enter the loop at all.
    currVarOp <- readVar var 
    currVarVal <- case currVarOp of { OpReg r -> return r; _ -> error "Loop var not reg" }
    
    bodyLabel <- freshLabel "for_body"
    
    let branchTerm = case dir of 
            Upwards   -> Branch Gt currVarVal (OpReg endReg) endLabel bodyLabel
            Downwards -> Branch Lt currVarVal (OpReg endReg) endLabel bodyLabel
            
    terminate branchTerm bodyLabel
    
    -- 7. [LOOP BODY]
    forM_ cmds genCommand
    
    -- 8. [LOOP LATCH] Check Bounds BEFORE Increment
    -- This prevents wrapping/saturation issues (e.g. 0 - 1 = 0) causing infinite loops.
    
    -- Get current i (potentially modified by body)
    valAtLatchOp <- readVar var
    valAtLatch <- case valAtLatchOp of { OpReg r -> return r; OpImm i -> do { t <- freshVReg; emit $ Move t (OpImm i); return t } }
    
    continueLabel <- freshLabel "for_continue"
    
    -- If i == end, we are done with this iteration AND the loop.
    -- (The condition logic assumes we execute inclusive range).
    terminate (Branch Eq valAtLatch (OpReg endReg) endLabel continueLabel) endLabel
    
    -- 9. [CONTINUE BLOCK] Increment/Decrement
    startBlock continueLabel
    
    newVal <- freshVReg
    let op = case dir of { Upwards -> OpAdd; Downwards -> OpSub }
    emit $ Compute op newVal (OpReg valAtLatch) (OpImm 1)
    writeVar var newVal
    
    -- 10. Jump back to Header
    bodyEndLabel <- gets currentLabel -- capture label of the continue block
    bodyEndEnv   <- gets varEnv       -- capture env after increment
    terminate (Jump loopLabel) endLabel
    
    -- 11. Backpatching Phis
    forM_ loopPhis $ \(pid, valPhi) -> do
        valBack <- getEnvVal pid bodyEndEnv
        updateBlock loopLabel $ \blk ->
            let updatePhi (Phi r ops) = 
                    if r == valPhi 
                    -- Important: The edge now comes from 'bodyEndLabel' (the continue block)
                    then Phi r (ops ++ [(bodyEndLabel, valBack)]) 
                    else Phi r ops
            in blk { blockPhis = map updatePhi (blockPhis blk) }

    -- 12. FIX: Restore environment to the Loop Header state
    modify $ \s -> s { varEnv = loopHeaderEnv }

genCondition :: Condition -> Label -> Label -> Codegen ()
genCondition cond trueLabel falseLabel = do
    (op, v1, v2) <- case cond of
        Equal a b     -> return (Eq, a, b)
        NEqual a b    -> return (Neq, a, b)
        Greater a b   -> return (Gt, a, b)
        Lesser a b    -> return (Lt, a, b)
        GreaterEq a b -> return (Ge, a, b)
        LesserEq a b  -> return (Le, a, b)
    val1 <- genValue v1
    val2 <- genValue v2
    r1 <- case val1 of { OpReg r -> return r; OpImm i -> do { t <- freshVReg; emit $ Move t (OpImm i); return t } }
    terminate (Branch op r1 val2 trueLabel falseLabel) trueLabel

mergeEnvironments :: Label -> VarEnv -> Label -> VarEnv -> Codegen ()
mergeEnvironments labelTrue envTrue labelFalse envFalse = do
    let allVars = nub $ Map.keys envTrue ++ Map.keys envFalse
    forM_ allVars $ \pid -> do
        let vTrue  = Map.lookup pid envTrue
        let vFalse = Map.lookup pid envFalse
        case (vTrue, vFalse) of
            (Just r1, Just r2) -> 
                if r1 /= r2 then do
                    rNew <- freshVReg
                    emitPhi $ Phi rNew [(labelTrue, OpReg r1), (labelFalse, OpReg r2)]
                    modify $ \s -> s { varEnv = Map.insert pid rNew (varEnv s) }
                else modify $ \s -> s { varEnv = Map.insert pid r1 (varEnv s) }
            (Just r1, Nothing) -> modify $ \s -> s { varEnv = Map.insert pid r1 (varEnv s) }
            (Nothing, Just r2) -> modify $ \s -> s { varEnv = Map.insert pid r2 (varEnv s) }
            _ -> return ()

scanModified :: [Command] -> [Pid]
scanModified [] = []
scanModified (ProcCall _ _ args : cs) = args ++ scanModified cs
scanModified (c:cs) = nub $ case c of
    Assignment _ (Scalar _ pid) _ -> pid : scanModified cs
    MyRead _ (Scalar _ pid)       -> pid : scanModified cs
    If _ _ c1                     -> scanModified c1 ++ scanModified cs
    IfElse _ _ c1 c2              -> scanModified c1 ++ scanModified c2 ++ scanModified cs
    While _ _ c1                  -> scanModified c1 ++ scanModified cs
    Repeat _ c1 _                 -> scanModified c1 ++ scanModified cs
    ForLoop _ pid _ _ _ c1        -> pid : scanModified c1 ++ scanModified cs
    _                             -> scanModified cs