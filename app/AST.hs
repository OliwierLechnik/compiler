-- Oliwier Lechnik 279760

module AST where

import Data.Text (Text)

type Pid = Text

type IntegerVal = Integer

data Type 
    = Ttype 
    | Itype 
    | Otype 
    deriving (Eq, Show)

data ProgramAll 
    = ProgramAll [Procedure] Main
    deriving (Eq, Show)

type Pos = Pos Int Int
    deriving (Eq, Show)

data Procedure 
    = Procedure Pid [(Maybe Type, Pid)] [Declaration] [Command]
    deriving (Eq, Show)

data Main 
    = Main [Declaration] [Command]
    deriving (Eq, Show)

data ForDir = Upwards | Downwards
--            TO      | DOWNTO
    deriving (Eq, Show)

data Command
    = Assignment Id Expr
    | ProcCall Pid [Pid]
    | IfElse Condition [Command] [Command]
    | If Condition [Command]
    | While Condition [Command]
    | Repeat [Command] Condition
    | ForLoop Pid Value Value ForDir [Command]
    | MyRead Id
    | MyWrite Value
    deriving (Eq, Show)

data Declaration
    = DeclScalar Pid
    | DeclArray Pid IntegerVal IntegerVal
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
    | NEqual Value Value
    | Greater Value Value
    | Lesser Value Value
    | GreaterEq Value Value
    | LesserEq Value Value
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