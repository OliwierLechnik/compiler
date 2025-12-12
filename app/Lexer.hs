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
    (L.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexNum :: Parser Integer
lexNum = lexeme L.decimal

symbol :: Text -> Parser Text
symbol = L.symbol sc

pidentifier :: Parser Pid -- [_a-z]+
pidentifier = lexeme $ takeWhile1P (Just "pidentifier") isAllowed
    where
        isAllowed c = isLower c || c == '_'

-- Lex Operators

type CondOp = (Value -> Value -> Condition)

lexCondOp :: Parser CondOp -- lex Condition Operator
lexCondOp = choice
    [ symbol "!=" *> pure NEqual
    , symbol ">=" *> pure GreaterEq
    , symbol "<=" *> pure LesserEq
    , symbol "="  *> pure Equal
    , symbol ">"  *> pure Greater
    , symbol "<"  *> pure Lesser
    ]

type ExprOp = (Value -> Value -> Expr)

lexExprOp :: Parser (Value -> Value -> Expr) -- lex Expr Operator
lexExprOp = choice
    [ symbol "+" *> pure Add
    , symbol "-" *> pure Sub
    , symbol "*" *> pure Mul
    , symbol "/" *> pure Div
    , symbol "%" *> pure Mod
    ]

lexAssignOp :: Parser Text
lexAssignOp = symbol ":="

-- Lex Keywords

keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy (alphaNumChar <|> char '_'))

lexTypeKeyword :: Parser Type
lexTypeKeyword = choice
    [ keyword "T" *> pure Ttype
    , keyword "I" *> pure Itype
    , keyword "O" *> pure Otype
    ]

lexForDir :: Parser ForDir
lexForDir = choice
    [ keyword "TO"     *> pure Upwards
    , keyword "DOWNTO" *> pure Downwards
    ]

-- Miscellaneous keywords

lexDo :: Parser Text
lexDo = keyword "DO"

lexElse :: Parser Text
lexElse = keyword "ELSE"

lexEnd :: Parser Text
lexEnd = keyword "END"

lexEndfor :: Parser Text
lexEndfor = keyword "ENDFOR"

lexEndif :: Parser Text
lexEndif = keyword "ENDIF"

lexEndwhile :: Parser Text
lexEndwhile = keyword "ENDWHILE"

lexFor :: Parser Text
lexFor = keyword "FOR"

lexFrom :: Parser Text
lexFrom = keyword "FROM"

lexIf :: Parser Text
lexIf = keyword "IF"

lexIn :: Parser Text
lexIn = keyword "IN"

lexIs :: Parser Text
lexIs = keyword "IS"

lexProcedure :: Parser Text
lexProcedure = keyword "PROCEDURE"

lexProgram :: Parser Text
lexProgram = keyword "PROGRAM"

lexRepeat :: Parser Text
lexRepeat = keyword "REPEAT"

lexThen :: Parser Text
lexThen = keyword "THEN"

lexUntil :: Parser Text
lexUntil = keyword "UNTIL"

lexWhile :: Parser Text
lexWhile = keyword "WHILE"

lexRead :: Parser Text
lexRead = keyword "READ"

lexWrite :: Parser Text
lexWrite = keyword "WRITE"