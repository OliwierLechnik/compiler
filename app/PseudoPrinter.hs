module PseudoPrinter where

import Data.List (intercalate)
import PseudoInstructions
import VMInterface (Register, Address)
import IR (VReg(..))
import qualified Data.Text as T

printPseudo :: [Pseudo] -> IO ()
printPseudo = mapM_ (putStrLn . formatPseudo)

formatPseudo :: Pseudo -> String
formatPseudo p = case p of
    P_READ          -> "READ"
    P_WRITE         -> "WRITE"
    P_LOAD t        -> "LOAD " ++ show t
    P_STORE t       -> "STORE " ++ show t
    P_RLOAD r       -> "RLOAD " ++ fmtReg r
    P_RSTORE r      -> "RSTORE " ++ fmtReg r
    P_ADD r         -> "ADD " ++ fmtReg r
    P_SUB r         -> "SUB " ++ fmtReg r
    P_SWP r         -> "SWP " ++ fmtReg r
    P_RST r         -> "RST " ++ fmtReg r
    P_INC r         -> "INC " ++ fmtReg r
    P_DEC r         -> "DEC " ++ fmtReg r
    P_SHL r         -> "SHL " ++ fmtReg r
    P_SHR r         -> "SHR " ++ fmtReg r
    P_JUMP t        -> "JUMP " ++ fmtTarget t
    P_JPOS t        -> "JPOS " ++ fmtTarget t
    P_JZERO t       -> "JZERO " ++ fmtTarget t
    P_CALL t        -> "CALL " ++ fmtTarget t
    P_RTRN          -> "RTRN"
    P_HALT          -> "HALT"
    P_LABEL l       -> "\n" ++ T.unpack l ++ ":" 

fmtReg :: PReg -> String
fmtReg (Left (VReg n)) = "v" ++ show n
fmtReg (Right r)       = show r

fmtTarget :: Target -> String
fmtTarget (T_Addr a)  = show a
fmtTarget (T_Label l) = T.unpack l