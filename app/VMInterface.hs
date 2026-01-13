-- Oliwier Lechnik 279760

module VMInterface where

data Register
    = RegA
    | RegB
    | RegC
    | RegD
    | RegE
    | RegF
    | RegG
    | RegH
    deriving (Eq, Enum, Show, Ord)

type Address = Integer

-- k is instruction pointer
data VMCommand ------------------------------------------------------------------------------
    = READ              --   | pobraną liczbę zapisuje w rejestrze `RegA` | k ← k + 1 | 100 |
    | WRITE             --   | wyświetla zawartość rejestru `RegA`        | k ← k + 1 | 100 |
    --------------------------------------------------------------------------------------- |
    | LOAD   Address    -- j | `RegA`← pj                                 | k ← k + 1 |  50 |
    | STORE  Address    -- j | pj ← `RegA`                                | k ← k + 1 |  50 |
    | RLOAD  Register   -- x | `RegA`← prx                                | k ← k + 1 |  50 |
    | RSTORE Register   -- x | prx ← `RegA`                               | k ← k + 1 |  50 |
    --------------------------------------------------------------------------------------- |
    | ADD    Register   -- x | `RegA`← `RegA`+ `RegX`                     | k ← k + 1 |   5 |
    | SUB    Register   -- x | `RegA`← max{ra − rx, 0}                    | k ← k + 1 |   5 |
    | SWP    Register   -- x | `RegA`↔ `RegX`                             | k ← k + 1 |   5 |
    --------------------------------------------------------------------------------------- |
    | RST    Register   -- x | `RegX`← 0                                  | k ← k + 1 |   1 |
    | INC    Register   -- x | `RegX`← `RegX`+ 1                          | k ← k + 1 |   1 |
    | DEC    Register   -- x | `RegX`← max{rx − 1, 0}                     | k ← k + 1 |   1 |
    | SHL    Register   -- x | `RegX`← 2 ∗ `RegX`                         | k ← k + 1 |   1 |
    | SHR    Register   -- x | `RegX`← ⌊rx/2⌋                             | k ← k + 1 |   1 |
    --------------------------------------------------------------------------------------- |
    | JUMP   Address    -- j | k ← j                                      |           |   1 |
    | JPOS   Address    -- j | jeśli `RegA`> 0 to k ← j p.p. k ← k + 1    |           |   1 |  
    | JZERO  Address    -- j | jeśli `RegA`= 0 to k ← j p.p. k ← k + 1    |           |   1 |  
    | CALL   Address    -- j | `RegA`← k + 1 oraz k ← j                   |           |   1 |  
    | RTRN              --   | k ← `RegA`                                 |           |   1 |  
    --------------------------------------------------------------------------------------- |
    | HALT              --   | zatrzymaj program                          |           |   0 |
    deriving (Eq, Show) ---------------------------------------------------------------------

vmCmdCost :: VMCommand -> Integer
vmCmdCost READ        = 100
vmCmdCost WRITE       = 100
vmCmdCost (LOAD  _)   = 50
vmCmdCost (STORE _)   = 50
vmCmdCost (RLOAD _)   = 50
vmCmdCost (RSTORE _)  = 50
vmCmdCost (ADD _ )    = 5
vmCmdCost (SUB _ )    = 5
vmCmdCost (SWP _ )    = 5
vmCmdCost (RST _ )    = 1
vmCmdCost (INC _ )    = 1
vmCmdCost (DEC _ )    = 1
vmCmdCost (SHL _ )    = 1
vmCmdCost (SHR _ )    = 1
vmCmdCost (JUMP _)    = 1
vmCmdCost (JPOS _)    = 1
vmCmdCost (JZERO _)   = 1
vmCmdCost (CALL _)    = 1
vmCmdCost RTRN        = 1
vmCmdCost HALT        = 0

naiveCost :: [VMCommand] -> Integer
naiveCost = sum . fmap vmCmdCost