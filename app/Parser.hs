-- Oliwier Lechnik 279760

{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST
import Lexer

import Text.Megaparsec

parseIdTail :: Parser (Either Pid IntegerVal)
parseIdTail = do
    _   <- symbol "["
    idx <- (Left <$> pidentifier) <|> (Right <$> lexNum)
    _   <- symbol "]"
    return idx

idCombine :: AST.Pos -> Pid -> Maybe (Either Pid IntegerVal) -> Id
idCombine pos idbase Nothing            = Scalar pos idbase
idCombine pos idbase (Just (Left pid))  = ArrayVar pos idbase pid
idCombine pos idbase (Just (Right n))   = ArrayConst pos idbase n

parseIdentifier :: Parser Id
parseIdentifier = do
    pos <- getSourcePos
    idbase <- pidentifier
    idtail   <- optional parseIdTail
    return (idCombine pos idbase idtail)

parseValue :: Parser Value
parseValue = (ValId <$> parseIdentifier) <|> (ValNum <$> lexNum)

parseCondition :: Parser Condition
parseCondition = do
    val1 <- parseValue
    op   <- lexCondOp
    val2 <- parseValue
    return (op val1 val2)

parseExpr :: Parser Expr
parseExpr = do
    val1 <- parseValue
    maybeOp <- optional $ do
        op   <- lexExprOp
        val2 <- parseValue
        return (op, val2)
    
    case maybeOp of
        Just (op, val2) -> return (op val1 val2)
        Nothing         -> return (Val val1)

parseDeclaration :: Parser Declaration
parseDeclaration = do 
    pos <- getSourcePos
    pid <- pidentifier
    maybeTail <- optional $ do
        _ <- symbol "["
        num1 <- lexNum
        _ <- symbol ":"
        num2 <- lexNum
        _ <- symbol "]"
        return (num1, num2)

    case maybeTail of
        Just (num1, num2) -> return (DeclArray pos pid num1 num2)
        Nothing         -> return (DeclScalar pos pid)

parseDeclarations :: Parser [Declaration]
parseDeclarations = parseDeclaration `sepBy` symbol ","

parseArgDecl :: Parser (Maybe Type, Pid)
parseArgDecl = do
    t <- optional lexTypeKeyword
    pid <- pidentifier
    return (t, pid)

parseArgsDecl :: Parser [(Maybe Type, Pid)]
parseArgsDecl = parseArgDecl `sepBy` symbol ","

-- COMMANDS --

parseWrite :: Parser Command
parseWrite = do
    pos <- getSourcePos
    _ <- lexWrite
    val <- parseValue
    _ <- symbol ";"
    return (MyWrite pos val)

parseRead :: Parser Command
parseRead = do
    pos <- getSourcePos
    _ <- lexRead
    pid <- parseIdentifier
    _ <- symbol ";"
    return (MyRead pos pid)

parseForLoop :: Parser Command
parseForLoop = do
    pos <- getSourcePos
    _ <- lexFor
    pid <- pidentifier
    _ <- lexFrom
    val1 <- parseValue
    dir <- lexForDir
    val2 <- parseValue
    _ <- lexDo
    commands <- parseCommands
    _ <- lexEndfor
    return (ForLoop pos pid val1 val2 dir commands)

parseRepeat :: Parser Command
parseRepeat = do
    pos <- getSourcePos
    _ <- lexRepeat
    commands <- parseCommands
    _ <- lexUntil
    condition <- parseCondition
    _ <- symbol ";"
    return (Repeat pos commands condition)

parseWhile :: Parser Command
parseWhile = do
    pos <- getSourcePos
    _ <- lexWhile
    cond <- parseCondition
    _ <- lexDo
    commands <- parseCommands
    _ <- lexEndwhile
    return (While pos cond commands)

parseIf :: Parser Command
parseIf = do

    pos <- getSourcePos
    _ <- lexIf
    cond <- parseCondition
    _ <- lexThen
    commands1 <- parseCommands
    maybeElse <- (Left <$> lexEndif) <|> (Right <$> parseElse)

    case maybeElse of
        Left _ -> return (If pos cond commands1)
        Right commands2 -> return(IfElse pos cond commands1 commands2)

    where
        parseElse :: Parser [Command]
        parseElse = do
            _ <- lexElse
            commands <- parseCommands
            _ <- lexEndif
            return commands

parseArgs :: Parser [Pid]
parseArgs = pidentifier `sepBy` symbol ","

parseAssAndCall :: Parser Command
parseAssAndCall = do

    pos <- getSourcePos
    pid <- pidentifier
    rest <- (Left <$> parseAss) <|> (Right <$> parseCall)
    _ <- symbol ";"
    
    case rest of
        Left (idTail, expr) -> return (Assignment pos (idCombine pos pid idTail) expr)
        Right args -> return (ProcCall pos pid args)

    where
        parseAss :: Parser (Maybe (Either Pid IntegerVal), Expr)
        parseAss = do
            mytail <- optional parseIdTail
            _ <- lexAssignOp
            expr <- parseExpr
            return (mytail, expr)
        parseCall :: Parser [Pid]
        parseCall = do 
            _ <- symbol "("
            args <- parseArgs
            _ <- symbol ")"
            return args

parseCommand :: Parser Command
parseCommand = choice
    [ parseWrite
    , parseRead
    , parseForLoop
    , parseRepeat
    , parseWhile
    , parseIf
    , parseAssAndCall
    ]

parseCommands :: Parser [Command]
parseCommands = many parseCommand

-- END COMMANDS --

parseProcedure :: Parser Procedure
parseProcedure = do
    pos <- getSourcePos
    _ <- lexProcedure
    name <- pidentifier
    _ <- symbol "("
    args <- parseArgsDecl
    _ <- symbol ")"
    _ <- lexIs
    decls <- parseDeclarations
    _ <- lexIn
    commands <- parseCommands
    _ <- lexEnd
    return (Procedure pos name args decls commands)

parseMain :: Parser Main
parseMain = do
    _ <- lexProgram
    _ <- lexIs
    decls <- parseDeclarations
    _ <- lexIn
    commands <- parseCommands
    _ <- lexEnd
    return (Main decls commands)

parseProgramAll :: Parser ProgramAll
parseProgramAll = do
    sc
    procs <- many parseProcedure
    m <- parseMain
    eof -- Ensure strict end of file
    return (ProgramAll procs m)