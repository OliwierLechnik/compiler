-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser (parseProgramAll)
import PrettyAST (printAST)
import Semantics

import qualified Data.Text.IO as TIO
import qualified System.Environment as SE

import Text.Megaparsec

main :: IO ()
main = do
    [name] <- SE.getArgs
    file <- TIO.readFile name
    let parseResult = runParser parseProgramAll name file
    case parseResult of 
        Left err -> putStrLn (errorBundlePretty err) >> return ()
        Right ast -> do
            print $ analyzeProgram ast
            printAST ast