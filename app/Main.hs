-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser (parseProgramAll)
import PrettyAST (printAST)
import Semantics

import qualified Data.Text.IO as TIO
import qualified System.Environment as SE
import qualified Data.Text as T

import Text.Megaparsec
import CodeGen
import GraphExporter
import PhiElimination
import Linearization
import PseudoPrinter
import RegisterNaiveAllocator
import LabelResolver
import VMInterface (exportProgram)
import Control.Monad (when)

import System.Exit (exitFailure)

makeRed :: String -> String
makeRed s = "\ESC[31m" ++ s ++ "\ESC[0m"

printError :: Error -> String
printError (ErrorVariableRedeclaration pos pid) =
    sourcePosPretty pos ++ " -> Identifier `" ++ T.unpack pid ++ "` is already declared"

printError (ErrorProcedureRedeclaration pos pid) =
    sourcePosPretty pos ++ " -> Identifier `" ++ T.unpack pid ++ "` is already declared"

printError (ErrorWriteToImmutableVariable pos pid) =
    sourcePosPretty pos ++ " -> Cannot write to immutable variable `" ++ T.unpack pid ++ "`"

printError (ErrorReadFromUninitializeVariable pos pid) =
    sourcePosPretty pos ++ " -> Variable `" ++ T.unpack pid ++ "` is read before being initialized"

printError (ErrorNotAScalar pos pid) =
    sourcePosPretty pos ++ " -> Identifier `" ++ T.unpack pid ++ "` is not a scalar"

printError (ErrorNotAnArray pos pid) =
    sourcePosPretty pos ++ " -> Identifier `" ++ T.unpack pid ++ "` is not an array"

printError (ErrorNotAProcedure pos pid) =
    sourcePosPretty pos ++ " -> Identifier `" ++ T.unpack pid ++ "` is not a procedure"

printError (ErrorProduceNotInScope pos pid) =
    sourcePosPretty pos ++ " -> Procedure `" ++ T.unpack pid ++ "` is not in scope"

printError (ErrorVariableNotInScope pos pid) =
    sourcePosPretty pos ++ " -> Variable `" ++ T.unpack pid ++ "` is not in scope"

printError (ErrorInvalidArguments pos pid) =
    sourcePosPretty pos ++ " -> Invalid arguments for procedure `" ++ T.unpack pid ++ "`"

main :: IO ()
main = do
    args <- SE.getArgs
    case args of
        [name, out] ->
            compileFile name out False

        [name, out, "cfg"] ->
            compileFile name out True

        _ -> do
            putStrLn "Usage: compiler <input.imp> <output.mr> [cfg]"
            exitFailure


compileFile :: FilePath -> FilePath -> Bool -> IO ()
compileFile name out dumpCFG = do
    file <- TIO.readFile name
    case runParser parseProgramAll name file of
        Left err ->
            putStrLn (errorBundlePretty err) >> exitFailure

        Right ast -> do
            let (GlobalState _ errors) = analyzeProgram ast
            mapM_ (putStrLn . makeRed . printError) errors

            let graph = genProgram ast

            when dumpCFG $
                saveGraph (out ++ ".cfg") graph

            let ssafree = eliminatePhis graph

            when dumpCFG $
                saveGraph (out ++ ".ssacfg") ssafree

            when dumpCFG $
                print ssafree

            let linear    = linearize ssafree
            when dumpCFG $
                printPseudo linear
            
            let allocated = allocate linear
            let resolved  = resolve allocated

            writeFile out (exportProgram resolved)
