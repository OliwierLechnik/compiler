-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module CodeGen where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)
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
    | LocRef   VM.Address 
    deriving (Show, Eq)

type SymTable = Map Pid VarLoc
type VarEnv = Map Pid VReg

data CodegenState = CodegenState
    { currentBlockInsns :: [Middle]      
    , currentBlockPhis  :: [Phi]         
    , finishedBlocks    :: Map Label Block
    , currentLabel      :: Label         
    , labelCount        :: Int           
    , vregCount         :: Integer       
    , freeMem           :: VM.Address    
    , symTable          :: SymTable
    , varEnv            :: VarEnv
    , procArgMap        :: Map (Pid, Int) VM.Address 
    , vregMap :: Map Integer Text -- Add this
    }

type Codegen a = State CodegenState a

initialState :: CodegenState
initialState = CodegenState
    { currentBlockInsns = []
    , currentBlockPhis  = []
    , finishedBlocks    = Map.empty
    , currentLabel      = "entry"
    , labelCount        = 0
    , vregCount         = 0
    , freeMem           = 0
    , symTable          = Map.empty
    , varEnv            = Map.empty
    , procArgMap        = Map.empty
    , vregMap           = Map.empty
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
            , blockPhis  = reverse (currentBlockPhis s) 
            , blockInsns = reverse (currentBlockInsns s)
            , blockTerm  = term 
            }
    modify $ \st -> st 
        { finishedBlocks = Map.insert (blockLabel blk) blk (finishedBlocks st)
        , currentLabel   = nextLabel
        , currentBlockInsns = [] 
        , currentBlockPhis  = [] 
        }

startBlock :: Label -> Codegen ()
startBlock lbl = modify $ \s -> s 
    { currentLabel = lbl
    , currentBlockInsns = []
    , currentBlockPhis = [] 
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

recordVReg :: VReg -> Pid -> Codegen ()
recordVReg (VReg id) name = 
    modify $ \s -> s { vregMap = Map.insert id name (vregMap s) }

writeVar :: Pid -> VReg -> Codegen ()
writeVar pid val = do
    recordVReg val pid
    exists <- checkVar pid
    if not exists then do
        addVar pid LocReg
        modify $ \s -> s { varEnv = Map.insert pid val (varEnv s) }
    else do
        loc <- getVarLoc pid
        case loc of
            LocReg -> modify $ \s -> s { varEnv = Map.insert pid val (varEnv s) }
            LocRef addr -> do
                ptrReg <- freshVReg
                emit $ Load ptrReg addr
                emit $ StoreIndirect ptrReg (OpReg val) 
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
        LocRef addr -> do
            ptrReg <- freshVReg
            emit $ Load ptrReg addr
            valReg <- freshVReg
            emit $ LoadIndirect valReg ptrReg
            return $ OpReg valReg
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

getArgAddr :: Pid -> Int -> Codegen VM.Address
getArgAddr pid idx = do
    m <- gets procArgMap
    case Map.lookup (pid, idx) m of
        Just addr -> return addr
        Nothing -> error $ "Argument " ++ show idx ++ " of proc " ++ show pid ++ " not found"

-- ===========================================================================
-- Code Generation
-- ===========================================================================

genProgram :: ProgramAll -> ProgramIR
genProgram (ProgramAll procs mainBlock) = 
    let finalState = execState (genAll procs mainBlock) initialState
        lastBlk = Block 
            { blockLabel = currentLabel finalState
            , blockPhis  = reverse (currentBlockPhis finalState)
            , blockInsns = reverse (currentBlockInsns finalState)
            , blockTerm  = Halt 
            }
        allBlocks = Map.insert (blockLabel lastBlk) lastBlk (finishedBlocks finalState)
    in ProgramIR allBlocks (vregMap finalState)

preAllocProcs :: [Procedure] -> Codegen ()
preAllocProcs procs = do
    forM_ procs $ \(Procedure _ pid args _ _) -> do
        zipWithM_ (\idx _ -> do
            addr <- allocMem 1
            modify $ \s -> s { procArgMap = Map.insert (pid, idx) addr (procArgMap s) }
            ) [0..] args
  where
    zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

genAll :: [Procedure] -> Main -> Codegen ()
genAll procs (Main decls cmds) = do
    preAllocProcs procs

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

    -- 1. Prologue
    retAddrReg <- freshVReg
    emit $ StoreRetAddr retAddrReg 
    
    -- 2. Declarations & Args
    zipWithM_ (\idx (_, argName) -> do
        addr <- getArgAddr pid idx
        addVar argName (LocRef addr)
        ) [0..] args
        
    forM_ decls genDeclaration
    forM_ cmds genCommand
    
    -- 3. Epilogue
    emit $ LoadRetAddr retAddrReg
    terminate Return "unreachable"
    
    modify $ \s -> s { symTable = oldSyms, varEnv = oldEnv }
  where
    zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

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
        LocRef paramAddr -> do
            ptrReg <- freshVReg
            emit $ Load ptrReg paramAddr
            return (OpReg ptrReg, True)
        LocReg -> error "Internal error: genAddr called on Register Scalar (should use writeVar/readVar)"
        LocArray addr _ -> return (OpImm (fromIntegral addr), False) -- If using array name as scalar

genAddr (ArrayConst _ pid idx) = do
    loc <- getVarLoc pid
    case loc of
        LocArray base start -> do
            let offset = idx - start
            return (OpImm (fromIntegral (base + offset)), False)
        LocRef paramAddr -> do
             baseReg <- freshVReg
             emit $ Load baseReg paramAddr
             finalAddr <- freshVReg
             emit $ Compute OpAdd finalAddr (OpReg baseReg) (OpImm idx)
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
        LocRef paramAddr -> do
            baseReg <- freshVReg
            emit $ Load baseReg paramAddr
            finalReg <- freshVReg
            emit $ Compute OpAdd finalReg (OpReg baseReg) idxVal
            return (OpReg finalReg, True)
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
    
    copyBackList <- zipWithM (\argPid idx -> do
        argSlotAddr <- getArgAddr name idx
        loc <- getVarLoc argPid
        case loc of
            LocReg -> do
                val <- readVar argPid
                tmpAddr <- allocMem 1
                case val of
                    OpReg r -> emit $ Store tmpAddr (OpReg r)
                    OpImm i -> emit $ Store tmpAddr (OpImm i)
                ptrReg <- freshVReg
                emit $ Move ptrReg (OpImm (fromIntegral tmpAddr))
                emit $ Store argSlotAddr (OpReg ptrReg)
                return $ Just (argPid, tmpAddr)
            LocArray base start -> do
                ptrReg <- freshVReg
                emit $ Move ptrReg (OpImm (fromIntegral base))
                emit $ Store argSlotAddr (OpReg ptrReg)
                return Nothing 
            LocRef refAddr -> do
                ptrReg <- freshVReg
                emit $ Load ptrReg refAddr
                emit $ Store argSlotAddr (OpReg ptrReg)
                return Nothing
        ) args [0..]
            
    emit $ Call label
    
    forM_ copyBackList $ \m -> case m of
        Just (pid, addr) -> do
            newVal <- freshVReg
            emit $ Load newVal addr
            writeVar pid newVal
        Nothing -> return ()
  where
    zipWithM f xs ys = sequence (zipWith f xs ys)

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
    let modVars = scanModified cmds
    ssaVars <- onlySSA modVars
    loopPhis <- forM ssaVars $ \pid -> do
        valEntry <- getEnvVal pid entryEnv
        valPhi <- freshVReg
        emitPhi $ Phi valPhi [(entryLabel, valEntry)]
        writeVar pid valPhi
        return (pid, valPhi)
    genCondition cond bodyLabel endLabel
    forM_ cmds genCommand
    bodyEndLabel <- gets currentLabel
    bodyEndEnv   <- gets varEnv
    terminate (Jump condLabel) endLabel
    forM_ loopPhis $ \(pid, valPhi) -> do
        valBack <- getEnvVal pid bodyEndEnv
        updateBlock condLabel $ \blk ->
            let updatePhi (Phi r ops) = 
                    if r == valPhi 
                    then Phi r (ops ++ [(bodyEndLabel, valBack)]) 
                    else Phi r ops
            in blk { blockPhis = map updatePhi (blockPhis blk) }

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

genCommand (ForLoop _ var startVal endVal dir cmds) = do
    startOp <- genValue startVal
    startReg <- freshVReg
    emit $ Move startReg startOp
    writeVar var startReg
    entryLabel <- gets currentLabel
    entryEnv   <- gets varEnv
    loopLabel <- freshLabel "for_loop"
    bodyLabel <- freshLabel "for_body"
    endLabel  <- freshLabel "for_end"
    terminate (Jump loopLabel) loopLabel
    let modVars = nub (var : scanModified cmds)
    ssaVars <- onlySSA modVars
    loopPhis <- forM ssaVars $ \pid -> do
        valEntry <- getEnvVal pid entryEnv
        valPhi <- freshVReg
        emitPhi $ Phi valPhi [(entryLabel, valEntry)]
        writeVar pid valPhi
        return (pid, valPhi)
    currVarOp <- readVar var 
    currVarVal <- case currVarOp of { OpReg r -> return r; _ -> error "Loop var not reg" }
    endOp <- genValue endVal
    let branchTerm = case dir of { Upwards -> Branch Gt currVarVal endOp endLabel bodyLabel; Downwards -> Branch Lt currVarVal endOp endLabel bodyLabel }
    terminate branchTerm bodyLabel
    forM_ cmds genCommand
    valToMod <- readVar var
    newVal <- freshVReg
    let op = case dir of { Upwards -> OpAdd; Downwards -> OpSub }
    case valToMod of { OpReg r -> emit $ Compute op newVal (OpReg r) (OpImm 1); OpImm i -> emit $ Compute op newVal (OpImm i) (OpImm 1) }
    writeVar var newVal
    bodyEndLabel <- gets currentLabel
    bodyEndEnv   <- gets varEnv
    terminate (Jump loopLabel) endLabel
    forM_ loopPhis $ \(pid, valPhi) -> do
        valBack <- getEnvVal pid bodyEndEnv
        updateBlock loopLabel $ \blk ->
            let updatePhi (Phi r ops) = 
                    if r == valPhi 
                    then Phi r (ops ++ [(bodyEndLabel, valBack)]) 
                    else Phi r ops
            in blk { blockPhis = map updatePhi (blockPhis blk) }

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
scanModified (c:cs) = nub $ case c of
    Assignment _ (Scalar _ pid) _ -> pid : scanModified cs
    MyRead _ (Scalar _ pid)       -> pid : scanModified cs
    If _ _ c1                     -> scanModified c1 ++ scanModified cs
    IfElse _ _ c1 c2              -> scanModified c1 ++ scanModified c2 ++ scanModified cs
    While _ _ c1                  -> scanModified c1 ++ scanModified cs
    Repeat _ c1 _                 -> scanModified c1 ++ scanModified cs
    ForLoop _ pid _ _ _ c1        -> pid : scanModified c1 ++ scanModified cs
    _                             -> scanModified cs