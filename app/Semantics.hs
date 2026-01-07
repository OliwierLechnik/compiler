-- Oliwier Lechnik 279760

module Semantics where

import AST
import SymbolTable

import Control.Monad.State
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

data Error
    = ErrorVariableRedeclaration Pos Pid
    | ErrorProcedureRedeclaration Pos Pid
    | ErrorWriteToImmutableVariable Pos Pid
    | ErrorReadFromUninitializeVariable Pos Pid
    | ErrorNotAScalar Pos Pid
    | ErrorNotAnArray Pos Pid
    | ErrorNotAProcedure Pos Pid
    | ErrorProduceNotInScope Pos Pid
    | ErrorVariableNotInScope Pos Pid
    | ErrorInvalidArguments Pos Pid
    deriving (Show, Eq)

data Warning
    = WarningReadingFromOType Pos Pid
    deriving (Show, Eq)

data GlobalState = GlobalState
    { freeMem :: Integer
    , errors  :: [Error]
    }
    deriving (Show)

type Scope = Map Pid SymbolEntry

addError :: Error -> State GlobalState ()
addError err =
    modify $ \st ->
        st { errors = err : errors st }

allocateMemory :: Integer -> State GlobalState Integer
allocateMemory n = do
    modify $ \st -> st { freeMem = freeMem st + n }
    gets freeMem

addArgument :: Pos -> (Maybe Type, Pid) -> Scope -> State GlobalState Scope -- reserve 1 memory for arguments, even for array because its passed by reference, so will just pass address of the variable
addArgument pos (mType, pid) scope = do
    when (Map.member pid scope) $ addError $ ErrorVariableRedeclaration pos pid
    addr <- allocateMemory 1
    let varInfo = case mType of
            Nothing       -> EntryVar $ InfoScalar addr True True
            Just Ttype    -> EntryVar $ InfoArray addr Nothing
            Just Itype    -> EntryVar $ InfoScalar addr True False
            Just Otype    -> EntryVar $ InfoScalar addr False True
    let newScope = Map.insert pid varInfo scope
    return newScope

addVariable :: Declaration -> Scope -> State GlobalState Scope
addVariable (DeclScalar pos pid) scope = do
    when (Map.member pid scope) $ addError $ ErrorVariableRedeclaration pos pid
    addr <- allocateMemory 1
    let varInfo = EntryVar $ InfoScalar addr True True
    let newScope = Map.insert pid varInfo scope
    return newScope
addVariable (DeclArray pos pid begin end) scope = do
    when (Map.member pid scope) $ addError $ ErrorVariableRedeclaration pos pid
    addr <- allocateMemory ( end - begin + 1 + 2 ) -- +2 for array header
    let varInfo = EntryVar $ InfoArray addr (Just $ ArrRange begin end)
    let newScope = Map.insert pid varInfo scope
    return newScope

analyzeId :: Id -> Scope -> State GlobalState ()
analyzeId (Scalar pos pid) scope = do
    let entry1 = Map.lookup pid scope
    case entry1 of
        Just (EntryVar (InfoScalar _ _ _)) -> return ()
        Nothing -> addError $ ErrorVariableNotInScope pos pid
        _ -> addError $ ErrorNotAScalar pos pid
analyzeId (ArrayVar pos pid1 pid2) scope = do
    let entry1 = Map.lookup pid1 scope
    let entry2 = Map.lookup pid2 scope

    case entry1 of
        Just (EntryVar (InfoArray _ _)) -> case entry2 of
            Just (EntryVar (InfoScalar _ _ _)) -> return ()
            Nothing -> addError $ ErrorVariableNotInScope pos pid2
            _ -> addError $ ErrorNotAScalar pos pid2
        Nothing -> addError $ ErrorVariableNotInScope pos pid1
        _ -> addError $ ErrorNotAnArray pos pid1
        
analyzeId (ArrayConst pos pid _) scope = do
    let entry1 = Map.lookup pid scope
    case entry1 of
        Just (EntryVar (InfoArray _ _)) -> return ()
        Nothing -> addError $ ErrorVariableNotInScope pos pid
        _ -> addError $ ErrorNotAnArray pos pid

-- Helper to check if a variable is mutable (used in Assignment and MyRead)
checkMutability :: Pos -> Id -> Scope -> State GlobalState ()
checkMutability pos idn scope = case idn of
    Scalar _ pid -> do
        case Map.lookup pid scope of
            Just (EntryVar (InfoScalar _ _ False)) -> addError $ ErrorWriteToImmutableVariable pos pid
            _ -> return ()
    _ -> return () -- All arrays are mutable

analyzeValue :: Value -> Scope -> State GlobalState ()
analyzeValue (ValNum _) _ = return ()
analyzeValue (ValId idn) scope = analyzeId idn scope

analyzeExpr :: Expr -> Scope -> State GlobalState ()
analyzeExpr (Val v) scope = analyzeValue v scope
analyzeExpr (Add v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeExpr (Sub v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeExpr (Mul v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeExpr (Div v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeExpr (Mod v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope

analyzeCond :: Condition -> Scope -> State GlobalState ()
analyzeCond (Equal v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeCond (NEqual v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeCond (Greater v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeCond (Lesser v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeCond (GreaterEq v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope
analyzeCond (LesserEq v1 v2) scope = do
    analyzeValue v1 scope
    analyzeValue v2 scope

analyzeArg :: Pos -> Pid -> (Maybe Type, Pid) -> Scope -> State GlobalState Scope
analyzeArg pos givenPid (argType, _) scope = do
    case Map.lookup givenPid scope of
        Just (EntryVar (InfoScalar _ isinit ismutable)) -> do
            -- immutable variable can be passed as argument only if is Input type
            when ((not ismutable) && (argType /= Just Itype)) $ addError $ ErrorInvalidArguments pos givenPid
            -- uninitialized variable can be passed as argument only if is Output type
            when ((not isinit) && (argType /= Just Otype)) $ addError $ ErrorInvalidArguments pos givenPid
            when (argType == Just Ttype) $ addError $ ErrorInvalidArguments pos givenPid
            return ()
        Just (EntryVar (InfoArray _ _)) -> do
            when (argType /= Just Ttype) $ addError $ ErrorInvalidArguments pos givenPid
            return ()

        Just _  -> addError $ ErrorInvalidArguments pos givenPid
        Nothing -> addError $ ErrorVariableNotInScope pos givenPid 
    return scope

analyzeCommand :: Command -> Scope -> State GlobalState Scope
analyzeCommand (Assignment pos idn expr) scope = do
    analyzeId idn scope
    analyzeExpr expr scope
    checkMutability pos idn scope
    return scope

analyzeCommand (ProcCall pos pid args) scope = do
    
    case Map.lookup pid scope of
        Just (EntryProc (ProcInfo expectedArgs)) -> do
            when (length args /= length expectedArgs) $ addError $ ErrorInvalidArguments pos pid
            mapM_ (\(arg, expected) -> analyzeArg pos arg expected scope) $ zip args expectedArgs
        Just _  -> addError $ ErrorNotAProcedure pos pid
        Nothing -> addError $ ErrorProduceNotInScope pos pid 
    return scope

analyzeCommand (IfElse _ cond thenBody elseBody) scope = do
    analyzeCond cond scope
    mapM_ (\c -> analyzeCommand c scope) thenBody
    mapM_ (\c -> analyzeCommand c scope) elseBody

    return scope

analyzeCommand (If _ cond thenBody) scope = do
    analyzeCond cond scope
    mapM_ (\c -> analyzeCommand c scope) thenBody
    return scope

analyzeCommand (While _ cond body) scope = do
    analyzeCond cond scope
    mapM_ (\c -> analyzeCommand c scope) body
    return scope

analyzeCommand (Repeat _ body cond) scope = do
    mapM_ (\c -> analyzeCommand c scope) body
    analyzeCond cond scope
    return scope

analyzeCommand (ForLoop pos iterator start end _ body) scope = do
    analyzeValue start scope
    analyzeValue end scope
    
    when (Map.member iterator scope) $ addError $ ErrorVariableRedeclaration pos iterator
    
    -- Iterators are immutable as per language specification
    addr <- allocateMemory 1 
    let iterInfo = EntryVar $ InfoScalar addr True False
    let newScope = Map.insert iterator iterInfo scope
    
    mapM_ (\c -> analyzeCommand c newScope) body
    return scope

analyzeCommand (MyRead pos idn) scope = do
    analyzeId idn scope
    checkMutability pos idn scope
    return scope

analyzeCommand (MyWrite _ val) scope = do
    analyzeValue val scope
    return scope

analyzeProcedure :: Procedure -> Scope -> State GlobalState Scope
analyzeProcedure (Procedure pos name args decl commands) scope = do

    when (Map.member name scope) $ addError $ ErrorProcedureRedeclaration pos name

    scope1 <- foldM (\acc arg -> addArgument pos arg acc) scope args
    scope2 <- foldM (\acc dec -> addVariable dec acc) scope1 decl
    mapM_ (\c -> analyzeCommand c scope2) commands
    return (Map.insert name (EntryProc $ ProcInfo args) scope)

analyzeMain :: Main -> Scope -> State GlobalState ()
analyzeMain (Main decl commands) scope = do
    scope1 <- foldM (\acc dec -> addVariable dec acc) scope decl
    mapM_ (\c -> analyzeCommand c scope1) commands

getScope :: Scope
getScope = Map.empty

analyzeProgram :: ProgramAll -> GlobalState
analyzeProgram (ProgramAll procedures main) =
    execState analyze (GlobalState 0 [])
  where
    analyze :: State GlobalState ()
    analyze = do
        scope <- foldM (\s p -> analyzeProcedure p s) getScope procedures
        analyzeMain main scope
