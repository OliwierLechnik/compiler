-- Oliwier Lechnik 279760

module Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Char (isAlphaNum) 

import AST

type Parser = Parsec Void Text -- will add custom errors later

sc :: Parser ()
sc = L.space
    space1
    empty
    empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

number :: Parser Integer
number = lexeme L.decimal

symbol :: Text -> Parser Text
symbol = L.symbol sc

pidentifier :: Parser Text -- [_a-z]+
pidentifier = lexeme $ takeWhile1P (Just "pidentifier") isAllowed
    where
        isAllowed c = isLower c || c == '_'


