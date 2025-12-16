-- Oliwier Lechnik 279760

module PrettyAST where

import AST
import Data.Text (unpack)

-- Entry point
printAST :: ProgramAll -> IO ()
printAST ast = putStrLn $ goProgramAll ast 0

-- Helper to indent
indent :: Int -> String
indent n = replicate (n * 2) ' '

-- ProgramAll
goProgramAll :: ProgramAll -> Int -> String
goProgramAll (ProgramAll procs main') lvl =
    indent lvl ++ "ProgramAll\n" ++
    concatMap (\p -> goProcedure p (lvl + 1)) procs ++
    goMain main' (lvl + 1)

-- Procedure
goProcedure :: Procedure -> Int -> String
goProcedure (Procedure _ pid args decls cmds) lvl =
    indent lvl ++ "Procedure: " ++ unpack pid ++ "\n" ++
    indent (lvl + 1) ++ "Args:\n" ++ concatMap (\(mty, pid') ->
        indent (lvl + 2) ++ show mty ++ " " ++ unpack pid' ++ "\n") args ++
    indent (lvl + 1) ++ "Declarations:\n" ++ concatMap (\d -> goDeclaration d (lvl + 2)) decls ++
    indent (lvl + 1) ++ "Commands:\n" ++ concatMap (\c -> goCommand c (lvl + 2)) cmds

-- Main
goMain :: Main -> Int -> String
goMain (Main decls cmds) lvl =
    indent lvl ++ "Main\n" ++
    indent (lvl + 1) ++ "Declarations:\n" ++ concatMap (\d -> goDeclaration d (lvl + 2)) decls ++
    indent (lvl + 1) ++ "Commands:\n" ++ concatMap (\c -> goCommand c (lvl + 2)) cmds

-- Declaration
goDeclaration :: Declaration -> Int -> String
goDeclaration (DeclScalar _ pid) lvl = indent lvl ++ "DeclScalar: " ++ unpack pid ++ "\n"
goDeclaration (DeclArray _ pid start end) lvl = 
    indent lvl ++ "DeclArray: " ++ unpack pid ++ "[" ++ show start ++ ".." ++ show end ++ "]\n"

-- Commands
goCommand :: Command -> Int -> String
goCommand (Assignment _ ident expr) lvl = indent lvl ++ "Assignment:\n" ++ goId ident (lvl + 1) ++ goExpr expr (lvl + 1)
goCommand (ProcCall _ pid args) lvl = indent lvl ++ "ProcCall: " ++ unpack pid ++ "(" ++ unwords (map unpack args) ++ ")\n"
goCommand (IfElse _ cond t f) lvl = indent lvl ++ "IfElse:\n" ++ goCondition cond (lvl + 1) ++
    indent (lvl + 1) ++ "Then:\n" ++ concatMap (\c -> goCommand c (lvl + 2)) t ++
    indent (lvl + 1) ++ "Else:\n" ++ concatMap (\c -> goCommand c (lvl + 2)) f
goCommand (If _ cond t) lvl = indent lvl ++ "If:\n" ++ goCondition cond (lvl + 1) ++
    indent (lvl + 1) ++ "Then:\n" ++ concatMap (\c -> goCommand c (lvl + 2)) t
goCommand (While _ cond cmds) lvl = indent lvl ++ "While:\n" ++ goCondition cond (lvl + 1) ++
    concatMap (\c -> goCommand c (lvl + 1)) cmds
goCommand (Repeat _ cmds cond) lvl = indent lvl ++ "Repeat:\n" ++ concatMap (\c -> goCommand c (lvl + 1)) cmds ++
    goCondition cond (lvl + 1)
goCommand (ForLoop _ pid start end dir cmds) lvl = indent lvl ++ "ForLoop: " ++ unpack pid ++ " " ++ show dir ++ "\n" ++
    indent (lvl + 1) ++ "From: " ++ show start ++ ", To: " ++ show end ++ "\n" ++
    concatMap (\c -> goCommand c (lvl + 1)) cmds
goCommand (MyRead _ ident) lvl = indent lvl ++ "Read:\n" ++ goId ident (lvl + 1)
goCommand (MyWrite _ val) lvl = indent lvl ++ "Write:\n" ++ goValue val (lvl + 1)

-- Expr
goExpr :: Expr -> Int -> String
goExpr (Val v) lvl = indent lvl ++ "Val:\n" ++ goValue v (lvl + 1)
goExpr (Add a b) lvl = indent lvl ++ "Add:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goExpr (Sub a b) lvl = indent lvl ++ "Sub:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goExpr (Mul a b) lvl = indent lvl ++ "Mul:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goExpr (Div a b) lvl = indent lvl ++ "Div:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goExpr (Mod a b) lvl = indent lvl ++ "Mod:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)

-- Condition
goCondition :: Condition -> Int -> String
goCondition (Equal a b) lvl = indent lvl ++ "Equal:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goCondition (NEqual a b) lvl = indent lvl ++ "NotEqual:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goCondition (Greater a b) lvl = indent lvl ++ "Greater:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goCondition (Lesser a b) lvl = indent lvl ++ "Lesser:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goCondition (GreaterEq a b) lvl = indent lvl ++ "GreaterEq:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)
goCondition (LesserEq a b) lvl = indent lvl ++ "LesserEq:\n" ++ goValue a (lvl + 1) ++ goValue b (lvl + 1)

-- Value
goValue :: Value -> Int -> String
goValue (ValNum n) lvl = indent lvl ++ "Num: " ++ show n ++ "\n"
goValue (ValId i) lvl = goId i lvl

-- Id
goId :: Id -> Int -> String
goId (Scalar _ pid) lvl = indent lvl ++ "Scalar: " ++ unpack pid ++ "\n"
goId (ArrayVar _ pid idx) lvl = indent lvl ++ "ArrayVar: " ++ unpack pid ++ "[" ++ unpack idx ++ "]\n"
goId (ArrayConst _ pid n) lvl = indent lvl ++ "ArrayConst: " ++ unpack pid ++ "[" ++ show n ++ "]\n"
