
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language.Tool.Parser
(
    toolParser
  , parseString
  , parseFile
)
where

import           Prelude              hiding (id)
import           Control.Applicative  hiding ((<|>), many, empty)

import           Text.Parsec
import           Text.Parsec.String

import           Language.Tool.AST
import           Language.Tool.Lexer

empty :: Parser ()
empty = return ()

lit :: String -> a -> Parser a
lit s t = reserved s >> return t

topLevel :: Parser a -> Parser a
topLevel = between whiteSpace eof

toolParser :: Parser Program
toolParser = topLevel parseProgram

parseString :: String -> Program
parseString str =
  case parse toolParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Program
parseFile file =
  do program  <- readFile file
     case parse toolParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

parseProgram :: Parser Program
parseProgram = do
  main    <- parseMainObject
  classes <- many parseClassDecl
  return $ Program main classes

parseMainObject :: Parser MainObject
parseMainObject = do
  reserved "object"
  id    <- parseIdent
  stmts <- braces (do
    reserved "def"
    reserved "main"
    parens empty
    colon
    reserved "Unit"
    symbol "="
    braces (many parseStmt))
  return $ MainObject id stmts

parseClassDecl :: Parser ClassDecl
parseClassDecl = do
  reserved "class"
  id     <- parseIdent
  parent <- optionMaybe (reserved "extends" >> parseIdent)
  symbol "{"
  vars   <- many parseVarDecl
  meths  <- many parseMethodDecl
  symbol "}"
  return $ ClassDecl id parent vars meths

parseVarDecl :: Parser VarDecl
parseVarDecl = do
  reserved "var"
  id <- parseIdent
  colon
  tpe <- parseType
  semi
  return $ VarDecl id tpe

parseMethodDecl :: Parser MethodDecl
parseMethodDecl = do
  reserved "def"
  name <- parseIdent
  args <- parens $ parseFormal `sepBy` comma
  colon
  retType <- parseType
  symbol "="
  symbol "{"
  vars <- many parseVarDecl
  stmts <- many parseStmt
  reserved "return"
  retExpr <- parseExpr
  semi
  symbol "}"
  return $ MethodDecl name retType args vars stmts retExpr

parseFormal :: Parser Formal
parseFormal = do
  id <- parseIdent
  colon
  tpe <- parseType
  return $ Formal id tpe

parseType :: Parser Type
parseType =  parseIntType
         <|> (reserved "Bool" >> return BoolType)
         <|> (reserved "String" >> return StringType)
         <|> ClassType <$> parseIdent

parseIntType :: Parser Type
parseIntType = do
  reserved "Int"
  (brackets empty >> return IntArrayType) <|> (return IntType)

parseStmt :: Parser Stmt
parseStmt =  parseBlock
    <|> parseIf
    <|> parseWhile
    <|> parsePrintln
    <|> try parseAssign
    <|> try parseArrayAssign

parseBlock :: Parser Stmt
parseBlock = do
  stmt <- braces (many parseStmt)
  return $ Block stmt

parseIf :: Parser Stmt
parseIf = do
  reserved "if"
  cond <- parens parseExpr
  thn <- parseStmt
  els <- optionMaybe (reserved "else" >> parseStmt)
  return $ If cond thn els

parseWhile :: Parser Stmt
parseWhile = do
  reserved "while"
  cond <- parens parseExpr
  stmt <- parseStmt
  return $ While cond stmt

parsePrintln :: Parser Stmt
parsePrintln = do
  reserved "println"
  t <- parens parseExpr
  semi
  return $ Println t

parseAssign :: Parser Stmt
parseAssign = do
  id <- parseIdent
  symbol "="
  val <- parseExpr
  semi
  return $ Assign id val

parseArrayAssign :: Parser Stmt
parseArrayAssign = do
  id <- parseIdent
  idx <- brackets parseExpr
  symbol "="
  val <- parseExpr
  semi
  return $ ArrayAssign id idx val

parseExpr :: Parser Expr
parseExpr = parseTerm >>= parseExprRest

parseExprRest :: Expr -> Parser Expr
parseExprRest t =
      try (parseArrayRead t)
  <|> try (parseArrayLength t)
  <|> try (parseMethodCall t)
  <|> return t

parseTerm :: Parser Expr
parseTerm =
      (lit "true"  TrueLit)
  <|> (lit "false" FalseLit)
  <|> (lit "this"  This)
  <|> IntLit     <$> integer
  <|> StringLit  <$> stringLiteral
  <|> Identifier <$> parseIdent
  <|> try parseNewIntArray
  <|> try parseNew
  <|> (symbol "!" >> Not <$> parseExpr)
  <|> parens parseExpr

parseArrayRead :: Expr -> Parser Expr
parseArrayRead arr = do
  idx <- brackets parseExpr
  parseExprRest $ ArrayRead arr idx

parseArrayLength :: Expr -> Parser Expr
parseArrayLength arr = do
  dot
  reserved "length"
  parseExprRest $ ArrayLength arr

parseMethodCall :: Expr -> Parser Expr
parseMethodCall obj = do
  dot
  meth <- parseIdent
  args <- parens (parseExpr `sepBy` comma)
  parseExprRest $ MethodCall obj meth args

parseNew :: Parser Expr
parseNew = do
  symbol "new"
  id <- parseIdent
  parens empty
  return $ New id

parseNewIntArray :: Parser Expr
parseNewIntArray = do
  symbol "new"
  reserved "Int"
  size <- brackets parseExpr
  return $ NewIntArray size

parseIdent :: Parser Ident
parseIdent = Ident <$> identifier

