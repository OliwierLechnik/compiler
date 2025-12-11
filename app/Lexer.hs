-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Char

import AST

type Parser = Parsec Void Text -- will add custom errors later

sc :: Parser ()
sc = L.space
    space1
    empty
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexNum :: Parser Integer
lexNum = lexeme L.decimal

symbol :: Text -> Parser Text
symbol = L.symbol sc

pidentifier :: Parser Text -- [_a-z]+
pidentifier = lexeme $ takeWhile1P (Just "pidentifier") isAllowed
    where
        isAllowed c = isLower c || c == '_'

-- Lex Operators

lexCondOp :: Parser (Value -> Value -> Condition) -- lex Condition Operator
lexCondOp = choice
    [ string "==" *> pure Equal
    , string "!=" *> pure NEqual
    , string ">=" *> pure GreaterEq
    , string "<=" *> pure LesserEq
    , string ">"  *> pure Greater
    , string "<"  *> pure Lesser
    ]

lexExprOp :: Parser (Value -> Value -> Expr) -- lex Expr Operator
lexExprOp = choice
    [ string "+" *> pure Add
    , string "-" *> pure Sub
    , string "*" *> pure Mul
    , string "/" *> pure Div
    , string "%" *> pure Mod
    ]

-- Lex Keywords

keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy alphaNumChar)

lexType :: Parser Type
lexType = choice
    [ keyword "T" *> pure Ttype
    , keyword "I" *> pure Itype
    , keyword "O" *> pure Otype
    , pure Nothingtype -- ? is that alright? is that how express that if no type token was found to return Nothingtype?
    ]

lexForDir :: Parser ForDir
lexForDir = choice
    [ keyword "TO"     *> pure Upwards
    , keyword "DOWNTO" *> pure Downwards
    ]

-- Miscellaneous keywords

