-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}

module Runtime where

import PseudoInstructions
import IR (VReg)
import Data.Text (Text)

-- | Interface for generating fresh names if needed
class Monad m => TempGen m where
    freshV :: m VReg
    freshL :: Text -> m Text

-- | Template: Multiplication
-- genMul :: TempGen m => PReg -> PReg -> PReg -> m [Pseudo]
-- genMul vDest v1 v2 = return [] -- Placeholder (Not implemented yet)

-- | Template: Division
genDiv :: TempGen m => PReg -> PReg -> PReg -> m [Pseudo]
genDiv vDest v1 v2 = return [] -- Placeholder

-- | Template: Modulo
genMod :: TempGen m => PReg -> PReg -> PReg -> m [Pseudo]
genMod vDest v1 v2 = return [] -- Placeholder

-- libMul :: [Pseudo]
-- libMul = 
--     [ P_LABEL "__lib_mul"
    
--     -- Initialize Result (RegD) = 0
--     , P_RST (Right RegA)        -- A = 0
--     , P_SWP (Right RegD)        -- RegD = 0

--     , P_LABEL ".mul_loop"
    
--     -- 1. Check if y (RegC) == 0
--     , P_RST (Right RegA)
--     , P_ADD (OpReg (vr RegC))   -- Load y
--     , P_CMP (OpImm 0)           -- Compare with 0
--     , P_JE ".mul_end"           -- If y == 0, we are done

--     -- 2. Check if y is Odd (y & 1)
--     , P_RST (Right RegA)
--     , P_ADD (OpReg (vr RegC))   -- Load y
--     , P_AND (OpImm 1)           -- A = y & 1
--     , P_CMP (OpImm 0)           
--     , P_JE ".mul_skip_add"      -- If (y & 1) == 0, skip addition

--     -- 3. If Odd: Result = Result + x
--     , P_RST (Right RegA)
--     , P_ADD (OpReg (vr RegD))   -- Load Result
--     , P_ADD (OpReg (vr RegB))   -- Add x
--     , P_SWP (Right RegD)        -- Store Result

--     , P_LABEL ".mul_skip_add"

--     -- 4. Double x (x = x + x)
--     , P_RST (Right RegA)
--     , P_ADD (OpReg (vr RegB))   -- Load x
--     , P_ADD (OpReg (vr RegB))   -- Add x again
--     , P_SWP (Right RegB)        -- Store x

--     -- 5. Halve y (y = y >> 1)
--     , P_RST (Right RegA)
--     , P_ADD (OpReg (vr RegC))   -- Load y
--     , P_SHR (OpImm 1)           -- Shift Right by 1
--     , P_SWP (Right RegC)        -- Store y

--     , P_JMP ".mul_loop"

--     , P_LABEL ".mul_end"
--     -- 6. Move Result (RegD) to Return Register (RegA)
--     , P_RST (Right RegA)
--     , P_ADD (OpReg (vr RegD))
--     , P_RET
--     ]
--   where
--     -- Helper to create a VReg that maps to a specific Hardware Register
--     -- You likely have a constructor for this, e.g., Precolored or similar.
--     vr r = VRegMapped r