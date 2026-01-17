module LabelResolver (resolve) where

import PseudoInstructions
import VMInterface
import IR (Label, VReg)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

-- | Main function: Converts Pseudo-code with labels/registers into final VM bytecode.
resolve :: [Pseudo] -> [VMCommand]
resolve instrs = mapMaybe (toVM labelMap) instrs
  where
    labelMap = buildLabelMap instrs

-- ============================================================================
-- Pass 1: Build Symbol Table
-- ============================================================================

buildLabelMap :: [Pseudo] -> Map Label Address
buildLabelMap instrs = go 0 instrs Map.empty
  where
    go :: Address -> [Pseudo] -> Map Label Address -> Map Label Address
    go _ [] acc = acc
    -- If we hit a label, record current address, but DO NOT increment address
    go addr (P_LABEL lbl : rest) acc = go addr rest (Map.insert lbl addr acc)
    -- All other instructions occupy 1 slot (instruction pointer increments by 1)
    go addr (_ : rest) acc = go (addr + 1) rest acc

-- ============================================================================
-- Pass 2: Code Generation
-- ============================================================================

-- | Helper to extract Physical Register. 
-- Throws error if a Virtual Register is found (Allocation should be done).
resReg :: PReg -> Register
resReg (Right r) = r
resReg (Left v)  = error $ "Linker Error: Unresolved VReg remaining: " ++ show v

-- | Helper to resolve Target (Addr or Label).
resTgt :: Map Label Address -> Target -> Address
resTgt _ (T_Addr a)  = a
resTgt m (T_Label l) = case Map.lookup l m of
    Just addr -> addr
    Nothing   -> error $ "Linker Error: Undefined label '" ++ show l ++ "'"
resTgt _ (T_VReg v)  = error $ "Linker Error: Unresolved VReg in Target: " ++ show v

-- | Convert single instruction. Returns Nothing for P_LABEL (removes it).
toVM :: Map Label Address -> Pseudo -> Maybe VMCommand
toVM _ (P_LABEL _) = Nothing

-- IO
toVM _ P_READ  = Just READ
toVM _ P_WRITE = Just WRITE

-- Memory
toVM m (P_LOAD t)  = Just $ LOAD (resTgt m t)
toVM m (P_STORE t) = Just $ STORE (resTgt m t)

-- Register Moves
toVM _ (P_RLOAD r)  = Just $ RLOAD (resReg r)
toVM _ (P_RSTORE r) = Just $ RSTORE (resReg r)

-- Arithmetic & Logic
toVM _ (P_ADD r) = Just $ ADD (resReg r)
toVM _ (P_SUB r) = Just $ SUB (resReg r)
toVM _ (P_SWP r) = Just $ SWP (resReg r)
toVM _ (P_RST r) = Just $ RST (resReg r)
toVM _ (P_INC r) = Just $ INC (resReg r)
toVM _ (P_DEC r) = Just $ DEC (resReg r)
toVM _ (P_SHL r) = Just $ SHL (resReg r)
toVM _ (P_SHR r) = Just $ SHR (resReg r)

-- Control Flow
toVM m (P_JUMP t)  = Just $ JUMP (resTgt m t)
toVM m (P_JPOS t)  = Just $ JPOS (resTgt m t)
toVM m (P_JZERO t) = Just $ JZERO (resTgt m t)
toVM m (P_CALL t)  = Just $ CALL (resTgt m t)
toVM _ P_RTRN      = Just RTRN
toVM _ P_HALT      = Just HALT