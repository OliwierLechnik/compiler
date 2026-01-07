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
    [name] <- SE.getArgs
    file <- TIO.readFile name
    let parseResult = runParser parseProgramAll name file
    case parseResult of 
        Left err -> putStrLn (errorBundlePretty err) >> return ()
        Right ast -> do
            let (GlobalState _ e) = analyzeProgram ast
            printAST ast
            mapM_ (putStrLn . makeRed . printError) e
            