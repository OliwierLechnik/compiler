-- Oliwier Lechnik 279760

module SymbolTable where

import AST (Type, Pid)

data ArrRange = ArrRange Integer Integer
    deriving (Eq, Show)

data VarInfo 
    = InfoScalar 
        { memOffset :: Integer
        , isInit    :: Bool          -- Has it been written to? DEFAULT IS TRUE FOR ALL VARIABLES EXCEPT ARGUMENTS WITH Otype
        , isMutable :: Bool          -- False for FOR-loop iterators and arguments with Itype
        }
    | InfoArray 
        { memOffset :: Integer    -- Start memory address
        , range :: Maybe ArrRange
        }
    deriving (Eq, Show)

data ProcInfo = ProcInfo
    { procArgs :: [(Maybe Type, Pid)] -- Arguments signature
    }
    deriving (Eq, Show)

data SymbolEntry 
    = EntryVar VarInfo
    | EntryProc ProcInfo
    deriving (Eq, Show)
