-- Oliwier Lechnik 279760

module AST where

import Data.Text (Text)
import Text.Megaparsec.Pos (SourcePos)

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

type Pos = SourcePos

data Procedure 
    = Procedure Pos Pid [(Maybe Type, Pid)] [Declaration] [Command]
    deriving (Eq, Show)

data Main 
    = Main [Declaration] [Command]
    deriving (Eq, Show)

data ForDir = Upwards | Downwards
--            TO      | DOWNTO
    deriving (Eq, Show)

data Command
    = Assignment Pos Id Expr
    | ProcCall Pos Pid [Pid]
    | IfElse Pos Condition [Command] [Command]
    | If Pos Condition [Command]
    | While Pos Condition [Command]
    | Repeat Pos [Command] Condition
    | ForLoop Pos Pid Value Value ForDir [Command]
    | MyRead Pos Id
    | MyWrite Pos Value
    deriving (Eq, Show)

data Declaration
    = DeclScalar Pos Pid
    | DeclArray Pos Pid IntegerVal IntegerVal
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
    = Scalar Pos Pid
    | ArrayVar Pos Pid Pid
    | ArrayConst Pos Pid IntegerVal
    deriving (Eq, Show)