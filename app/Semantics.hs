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
    addr <- allocateMemory (end-begin+1)
    let varInfo = EntryVar $ InfoArray addr (Just $ ArrRange begin end)
    let newScope = Map.insert pid varInfo scope
    return newScope


analyzeCommand :: Command -> Scope -> State GlobalState Scope
-- analyzeCommand (Assignment pos id expr) = do
-- analyzeCommand (ProcCall pos id args) = do
-- analyzeCommand (IfElse pos cond commands1 commands) = do
-- analyzeCommand (If pos cond commands) = do
-- analyzeCommand (While pos cond commands) = do
-- analyzeCommand (Repea pos commands cond) = do
analyzeCommand (ForLoop pos iterator _ _ _ body) scope = do
    when (Map.member iterator scope) $ addError $ ErrorVariableRedeclaration pos iterator
    addr <- allocateMemory 1 
    let iterInfo = EntryVar $ InfoScalar addr True False
    let newScope = Map.insert iterator iterInfo scope
    mapM_ (\c -> analyzeCommand c newScope) body
-- analyzeCommand (MyRead pos id) = do
-- analyzeCommand (MyWrite pos val) = do

analyzeCommand _ _ = do
    return ()

analyzeProcedure :: Procedure -> Scope -> State GlobalState Scope
analyzeProcedure (Procedure pos name args decl commands) scope = do
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
