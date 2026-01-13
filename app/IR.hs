-- Oliwier Lechnik 279760

module IR where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import VMInterface (Register, Address)
import AST (IntegerVal)

type Label = Text

-- | Virtual Register
-- Simple Integer wrapper. CodeGen will increment a counter to generate fresh ones.
newtype VReg = VReg Integer
    deriving (Eq, Ord, Show)

-- | Operands
data Operand 
    = OpReg VReg       -- register containing a variable
    | OpImm IntegerVal -- constant value
    deriving (Eq, Show)

data BinOp = OpAdd | OpSub | OpMul | OpDiv | OpMod deriving (Eq, Show)
data CondOp = Eq | Neq | Gt | Lt | Ge | Le deriving (Eq, Show)

-- | Phi Node (SSA)
-- "Phi DestReg [(PredecessorLabel, SourceOperand), ...]"
-- Indicates that 'DestReg' gets the value of 'SourceOperand' if control 
-- flow arrived from 'PredecessorLabel'.
data Phi = Phi VReg [(Label, Operand)]
    deriving (Eq, Show)

data Middle
    = Move VReg Operand                  -- copies operand to provided virtual register           
    | Compute BinOp VReg Operand Operand -- vreg <- Op1 binop Op2
    | Load VReg Address                  -- vreg <- Mem[Address] (Direct Load - for Scalars)
    | Store Address Operand              -- Mem[Address] = Operand (Direct Store - for Scalars)
    | LoadIndirect VReg VReg             -- vreg <- Mem[vreg] (Indirect Load - for Array Access)
    | StoreIndirect VReg Operand         -- Mem[vreg] = Operand (Indirect Store - for Array Access)
    | Print VReg                         -- Print the value of vreg
    | ReadInput VReg                     -- vreg <- <input from terminal>
    | Call Label                         -- Call procedure at Label. 
    | StoreRetAddr VReg                  -- vreg <- RegA (Capture Return Address at entry)
    | LoadRetAddr VReg                   -- RegA <- vreg (Restore Return Address before exit)
    | Comment Text
    deriving (Eq, Show)

data Terminator
    = Jump Label
    | Branch CondOp VReg Operand Label Label -- If v1 op v2 goto L1 else L2
    | Return       -- Return from procedure
                   -- Note: Since this is a Terminator, it implies the block ends here.
                   -- The compiler must generate a continuation block for the return address.
    | Halt         -- Halt the program
    deriving (Eq, Show)

-- Map VReg ID -> Original Variable Name (for debugging/graphing)
type DebugMap = Map Integer Text 

-- | Basic Block
-- Structure:
-- 1. Label
-- 2. Phi Nodes (Parallel execution, logic selection based on predecessor)
-- 3. Middle Instructions (Sequential execution)
-- 4. Terminator (Control flow transfer)
data Block = Block
    { blockLabel :: Label
    , blockPhis  :: [Phi]     -- ^ NEW: Phi nodes at the start of the block
    , blockInsns :: [Middle]
    , blockTerm  :: Terminator
    }
    deriving (Eq, Show)

-- | ProgramIR = Map of Labels to Blocks
data ProgramIR = ProgramIR 
    { getBlocks   :: Map Label Block
    , getDebugMap :: DebugMap -- <--- NEW
    } deriving (Eq, Show)

emptyProgramIR :: ProgramIR
emptyProgramIR = ProgramIR Map.empty Map.empty

addBlock :: Block -> ProgramIR -> ProgramIR
addBlock b (ProgramIR m a) = ProgramIR (Map.insert (blockLabel b) b m) a

-- Used for sorting blocks for linear output
blocks :: ProgramIR -> [Block]
blocks (ProgramIR m _) = Map.elems m