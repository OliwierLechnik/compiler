-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}

module Main where

import AST
import Parser
import PrettyAST

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void
import Data.Char

main :: IO ()
main = do
    let name = "testy2025/example1.imp"
    file <- TIO.readFile name
    print file
    let parseResult = runParser parseProgramAll name file
    case parseResult of 
        Left err -> putStrLn (errorBundlePretty err)
        Right ast -> printAST ast
