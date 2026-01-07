-- Oliwier Lechnik 279760

module Translation where

import VMInterface

generateConst :: Integer -> Register -> [VMCommand] -- generates a constant on a given register
generateConst = go [] where
    go :: [VMCommand] -> Integer -> Register -> [VMCommand]
    go x 0 _ = x
    go x y reg
        | even y                     = go ((SHL reg) : x) (y `div` 2) reg
        | (y `mod` 4) == 3 && y /= 3 = go ((DEC reg) : x) (y + 1) reg
        | otherwise                  = go ((INC reg) : x) (y - 1) reg

-- generateMultiplication :: Register -> Register -> [VMCommand] -- multiplies Reg_X by Reg_Y, stores result in RegA
-- generateMultiplication regx regy =