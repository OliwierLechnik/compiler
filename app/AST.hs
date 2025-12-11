-- Oliwier Lechnik 279760

module AST where

import Data.Text (Text)

type Pid = Text

type IntegerVal = Integer

data Type 
    = Ttype 
    | Itype 
    | Otype 
    | Nothingtype
    deriving (Eq, Show)

data Program_all 
    = Program_all [Procedure] Main
    deriving (Eq, Show)

data Procedure 
    = Procedure Pid [(Type, Pid)] (Maybe Declarations) [Command]
    deriving (Eq, Show)

data Main 
    = Main (Maybe Declarations) [Command]
    deriving (Eq, Show)

data ForDir = To | Downto 
    deriving (Eq, Show)


data Command
    = Assignment Id Expr
    | Proccall Pid [Pid]
    | Ifelse Condition [Command] [Command]
    | If Condition [Command]
    | While Condition [Command]
    | Repeat [Command] Condition
    | For Pid Value ForDir Value [Command]
    | Myread Id
    | Mywrite Value
    deriving (Eq, Show)

type Declarations = [Declaration]

data Declaration
    = Declscalar Pid
    | Declarray Pid IntegerVal IntegerVal
    deriving (Eq, Show)

data Expr
    = Val Value
    | Add Value Value
    | Sub Value Value
    | Mul Value Value
    | Div Value Value
    | Mod Value Value
    deriving (Eq, Show)

data Condition
    = Equal Value Value
    | Nequal Value Value
    | Greater Value Value
    | Lesser Value Value
    | Greatereq Value Value
    | Lessereq Value Value
    deriving (Eq, Show)

data Value 
    = ValNum IntegerVal 
    | ValId Id
    deriving (Eq, Show)

data Id
    = Scalar Pid
    | ArrayVar Pid Pid
    | ArrayConst Pid IntegerVal
    deriving (Eq, Show)