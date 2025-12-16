data Register
    = RegA
    | RegB
    | RegC
    | RegD
    | RegE
    | RegF
    | RegG
    | RegH
    deriving (Eq, Enum, Show)

data VMCommand
    = SHL Register
    | INC Register
    | DEC Register
    deriving (Eq, Show)


generateConst :: Integer -> Register -> [VMCommand]
generateConst = helper [] where
    helper :: [VMCommand] -> Integer -> Register -> [VMCommand]
    helper x 0 _ = x
    helper x y reg
        | even y                     = helper ((SHL reg) : x) (y `div` 2) reg
        | (y `mod` 4) == 3 && y /= 3 = helper ((DEC reg) : x) (y + 1) reg
        | otherwise                  = helper ((INC reg) : x) (y - 1) reg
