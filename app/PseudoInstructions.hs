-- Oliwier Lechnik 279760

module PseudoInstructions where

import Data.Text (Text)
import IR (VReg, Label)
import VMInterface (Register, Address)

-- | PReg: Register Operand (Virtual or Physical)
type PReg = Either VReg Register

-- | Target: Jump Target or Address
-- Includes T_VReg for abstract Load/Store before allocation
data Target 
    = T_Addr Address 
    | T_Label Label 
    | T_VReg VReg
    deriving (Eq, Show)

-- | Pseudo-Instructions
-- Isomorphic to VMCommand but uses PReg/Target and includes Label
data Pseudo
    = P_READ
    | P_WRITE
    | P_LOAD Target
    | P_STORE Target
    | P_RLOAD PReg
    | P_RSTORE PReg
    | P_ADD PReg
    | P_SUB PReg
    | P_SWP PReg
    | P_RST PReg
    | P_INC PReg
    | P_DEC PReg
    | P_SHL PReg
    | P_SHR PReg
    | P_JUMP Target
    | P_JPOS Target
    | P_JZERO Target
    | P_CALL Target
    | P_RTRN
    | P_HALT
    | P_LABEL Label
    deriving (Eq, Show)