-- Oliwier Lechnik 279760

module Codegen where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import AST (Pid, IntegerVal, Type(..))
import IR -- Import your IR types
import qualified VMInterface as VM -- For Address type

-- 1. Where is a variable stored?
data VarLoc
    = LocScalar VM.Address          -- Local Scalar or Scalar Argument
    | LocArray  VM.Address Integer Integer -- Local Array (Addr, Start, End)
    | LocArgRef VM.Address          -- 'T' Argument (Pointer to array)
    deriving (Show, Eq)

-- 2. The Scope (Map Name -> Location)
type SymTable = Map Pid VarLoc

-- 3. The CodeGen State
data CodegenState = CodegenState
    { -- Builder State
      currentBlock :: [Middle]       -- Instructions in current block (reversed)
    , finishedBlocks :: Map Label Block -- Completed blocks
    , currentLabel :: Label          -- Label of the current block
    
    -- Counters
    , labelCount :: Int              -- To generate L_1, L_2
    , vregCount  :: Int              -- To generate v1, v2
    , freeMem    :: VM.Address       -- Re-calculated memory offset
    
    -- Context
    , symTable   :: SymTable         -- The active symbol table
    }

-- 4. The Monad
type Codegen a = State CodegenState a

-- 5. Helpers
emit :: Middle -> Codegen ()
emit instr = modify $ \s -> s { currentBlock = instr : currentBlock s }

-- 6. Code Generation
genDeclArray :: Pid -> Integer -> Integer -> Codegen ()
genDeclArray pid start end = do
    baseAddr <- allocateMem (end - start + 1 + 2)
    
    -- Emit IR to initialize header
    -- Mem[base] = start
    emit $ Store baseAddr (OpImm start)
    -- Mem[base+1] = end
    emit $ Store (baseAddr + 1) (OpImm end)
    
    -- Update Symbol Table
    modify $ \s -> s { symTable = Map.insert pid (LocArray baseAddr start end) (symTable s) }

withScope :: Codegen a -> Codegen a
withScope action = do
    oldTable <- gets symTable
    res <- action
    modify $ \s -> s { symTable = oldTable } -- Restore old table (pop scope)
    -- Note: We do NOT restore freeMem. Memory usage persists.
    return res