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
    = OpReg VReg 
    | OpImm IntegerVal
    deriving (Eq, Show)

data BinOp = OpAdd | OpSub | OpMul | OpDiv | OpMod deriving (Eq, Show)
data CondOp = Eq | Neq | Gt | Lt | Ge | Le deriving (Eq, Show)

data Middle
    -- | v1 = Operand (Copies an operand into provided register)
    = Move VReg Operand                 
    
    -- | v1 = Op1 binop Op2
    | Compute BinOp VReg Operand Operand 
    
    -- | v1 = Mem[Address] (Direct Load - for Scalars)
    | Load VReg Address                 
    
    -- | Mem[Address] = Operand (Direct Store - for Scalars)
    | Store Address Operand                
    
    -- | v1 = Mem[v2] (Indirect Load - for Array Access)
    | LoadIndirect VReg VReg            
    
    -- | Mem[v1] = Operand (Indirect Store - for Array Access)
    | StoreIndirect VReg Operand           
    
    -- | Call a procedure
    | Call Label
    
    -- | IO
    | Print Operand
    | ReadInput VReg
    
    -- | Debugging / Comments
    | Comment Text
    deriving (Eq, Show)

data Terminator
    = Jump Label
    | Branch CondOp VReg Operand Label Label -- If v1 op v2 goto L1 else L2
    | Return
    | Halt
    deriving (Eq, Show)

-- | Basic Block
data Block = Block
    { blockLabel :: Label
    , blockInsns :: [Middle]
    , blockTerm  :: Terminator
    }
    deriving (Eq, Show)

-- | Program = Map of Labels to Blocks
newtype Program = Program (Map Label Block)
    deriving (Eq, Show)

emptyProgram :: Program
emptyProgram = Program Map.empty

addBlock :: Block -> Program -> Program
addBlock b (Program m) = Program (Map.insert (blockLabel b) b m)

-- Used for sorting blocks for linear output
blocks :: Program -> [Block]
blocks (Program m) = Map.elems m