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

idCombine :: Pid -> Maybe (Either Pid IntegerVal) -> Id
idCombine idbase Nothing            = Scalar idbase
idCombine idbase (Just (Left pid))  = ArrayVar idbase pid
idCombine idbase (Just (Right n))   = ArrayConst idbase n

parseIdentifier :: Parser Id
parseIdentifier = do
    idbase <- pidentifier
    idtail   <- optional parseIdTail
    return (idCombine idbase idtail)


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
    pid <- pidentifier
    maybeTail <- optional $ do
        _ <- symbol "["
        num1 <- lexNum
        _ <- symbol ":"
        num2 <- lexNum
        _ <- symbol "]"
        return (num1, num2)

    case maybeTail of
        Just (num1, num2) -> return (DeclArray pid num1 num2)
        Nothing         -> return (DeclScalar pid)

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
    _ <- lexWrite
    val <- parseValue
    _ <- symbol ";"
    return (MyWrite val)

parseRead :: Parser Command
parseRead = do
    _ <- lexRead
    pid <- parseIdentifier
    _ <- symbol ";"
    return (MyRead pid)

parseForLoop :: Parser Command
parseForLoop = do
    _ <- lexFor
    pid <- pidentifier
    _ <- lexFrom
    val1 <- parseValue
    dir <- lexForDir
    val2 <- parseValue
    _ <- lexDo
    commands <- parseCommands
    _ <- lexEndfor
    return (ForLoop pid val1 val2 dir commands)

parseRepeat :: Parser Command
parseRepeat = do
    _ <- lexRepeat
    commands <- parseCommands
    _ <- lexUntil
    condition <- parseCondition
    _ <- symbol ";"
    return (Repeat commands condition)

parseWhile :: Parser Command
parseWhile = do
    _ <- lexWhile
    cond <- parseCondition
    _ <- lexDo
    commands <- parseCommands
    _ <- lexEndwhile
    return (While cond commands)

parseIf :: Parser Command
parseIf = do
    _ <- lexIf
    cond <- parseCondition
    _ <- lexThen
    commands1 <- parseCommands
    maybeElse <- (Left <$> lexEndif) <|> (Right <$> parseElse)

    case maybeElse of
        Left _ -> return (If cond commands1)
        Right commands2 -> return(IfElse cond commands1 commands2)

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
    pid <- pidentifier
    rest <- (Left <$> parseAss) <|> (Right <$> parseCall)
    _ <- symbol ";"
    
    case rest of
        Left (idTail, expr) -> return (Assignment (idCombine pid idTail) expr)
        Right args -> return (ProcCall pid args)

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
parseCommand = choice [parseWrite, parseRead, parseForLoop, parseRepeat, parseWhile, parseIf, parseAssAndCall]

parseCommands :: Parser [Command]
parseCommands = many parseCommand

-- END COMMANDS --

parseProcedure :: Parser Procedure
parseProcedure = do
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
    return (Procedure name args decls commands)

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