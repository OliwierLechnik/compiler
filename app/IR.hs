-- Oliwier Lechnik 279760

module IR where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import VMInterface (Register, Address)
import AST (IntegerVal)

type Label = Text

newtype VReg = VReg Integer
    deriving (Eq, Ord, Show)

data Operand 
    = OpReg VReg       
    | OpImm IntegerVal 
    deriving (Eq, Show)

data BinOp = OpAdd | OpSub | OpMul | OpDiv | OpMod deriving (Eq, Show)
data CondOp = Eq | Neq | Gt | Lt | Ge | Le deriving (Eq, Show)

data Expression = Bin BinOp Expression Expression | Imm Operand
    deriving (Eq, Show)

data Phi = Phi VReg [(Label, Operand)]
    deriving (Eq, Show)

data Middle
    = Move VReg Operand                  
    | Compute BinOp VReg Operand Operand
    | Load VReg Address                  
    | Store Address Operand              
    | LoadIndirect VReg VReg             
    | StoreIndirect VReg Operand         
    | Print VReg                         
    | ReadInput VReg
    | Call Label [Operand]
    | StoreRetAddr VReg
    | LoadRetAddr VReg
    | Comment Text
    deriving (Eq, Show)

data Terminator
    = Jump Label
    | Branch CondOp VReg Operand Label Label 
    | Return
    | Halt         
    deriving (Eq, Show)

data Block = Block
    { blockLabel :: Label
    , blockArgs  :: [VReg]
    , blockPhis  :: [Phi]     
    , blockInsns :: [Middle]
    , blockTerm  :: Terminator
    }
    deriving (Eq, Show)

newtype ProgramIR = ProgramIR (Map Label Block)
    deriving (Eq, Show)

emptyProgramIR :: ProgramIR
emptyProgramIR = ProgramIR Map.empty

addBlock :: Block -> ProgramIR -> ProgramIR
addBlock b (ProgramIR m) = ProgramIR (Map.insert (blockLabel b) b m)

blocks :: ProgramIR -> [Block]
blocks (ProgramIR m) = Map.elems m