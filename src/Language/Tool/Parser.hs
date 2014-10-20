
{-# OPTIONS_GHC -Wall
                -fno-warn-missing-signatures
                -fno-warn-unused-do-bind #-}

module Language.Tool.Parser
(
    Program (..)
  , MainObject (..)
  , ClassDecl (..)
  , VarDecl (..)
  , MethodDecl (..)
  , Formal (..)
  , Type (..)
  , Stmt (..)
  , Expr (..)
  , toolParser
  , parseString
  , parseFile
)
where

import           Prelude              hiding (id)
import           Control.Applicative  hiding ((<|>), many, empty)
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as T

data Program =
  Program MainObject
          [ClassDecl]
  deriving (Show)

data MainObject =
  MainObject Ident
             [Stmt]
  deriving (Show)

data ClassDecl =
  ClassDecl Ident
            (Maybe Ident)
            [VarDecl]
            [MethodDecl]
  deriving (Show)

data VarDecl =
  VarDecl Ident
          Type
  deriving (Show)

data MethodDecl =
  MethodDecl Ident
             Type
             [Formal]
             [VarDecl]
             [Stmt]
             Expr
  deriving (Show)

data Formal =
  Formal Ident
         Type
  deriving (Show)

data Type =
    IntArrayType
  | IntType
  | BoolType
  | StringType
  | ClassType Ident
  deriving (Show)

data Stmt =
    Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  | Println Expr
  | Assign Ident Expr
  | ArrayAssign Ident Expr Expr
  deriving (Show)

data Expr =
    And Expr Expr
  | Or Expr Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Div Expr Expr
  | LessThan Expr Expr
  | Equals Expr Expr
  | ArrayRead Expr Expr
  | ArrayLength Expr
  | MethodCall Expr Ident [Expr]
  | IntLit Integer
  | StringLit String
  | TrueLit
  | FalseLit
  | Identifier Ident
  | This
  | NewIntArray Expr
  | New Ident
  | Not Expr
  deriving (Show)

newtype Ident =
  Ident String
  deriving (Show)


toolDef :: LanguageDef st
toolDef =
  javaStyle { nestedComments  = False
            , reservedNames   = [ "object", "class", "def", "var"
                                , "Unit", "main", "String", "extends"
                                , "Int", "Bool", "while", "if", "else"
                                , "return", "length", "true", "false"
                                , "this", "new", "println" ]
            , reservedOpNames = [ "+", "-", "*", "/"
                                , "<", "||", "&&", "!" ]
            , caseSensitive   = True
            }

lexer = Token.makeTokenParser toolDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
colon      = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer
braces     = Token.braces     lexer
brackets   = Token.brackets   lexer
symbol     = Token.symbol     lexer

parseTool :: Parser Program
parseTool = whiteSpace >> program

program :: Parser Program
program = do
  main    <- mainObject
  classes <- many classDecl
  return $ Program main classes

mainObject :: Parser MainObject
mainObject = do
  reserved "object"
  id    <- ident
  stmts <- braces (do
    reserved "def"
    reserved "main"
    colon
    reserved "Unit"
    symbol "="
    braces (many stmt))
  return $ MainObject id stmts

classDecl :: Parser ClassDecl
classDecl = do
  reserved "class"
  id     <- ident
  parent <- optionMaybe (reserved "extends" >> ident)
  vars   <- many varDecl
  meths  <- many methodDecl
  return $ ClassDecl id parent vars meths

varDecl :: Parser VarDecl
varDecl = do
  reserved "var"
  id <- ident
  colon
  tpe <- parseType
  semi
  return $ VarDecl id tpe

methodDecl :: Parser MethodDecl
methodDecl = do
  reserved "def"
  name <- ident
  args <- parens $ formal `sepBy` symbol ","
  colon
  retType <- parseType
  symbol "="
  symbol "{"
  vars <- many varDecl
  stmts <- many stmt
  reserved "return"
  retExpr <- expr
  semi
  symbol "}"
  return $ MethodDecl name retType args vars stmts retExpr

formal :: Parser Formal
formal = do
  id <- ident
  colon
  tpe <- parseType
  return $ Formal id tpe

parseType :: Parser Type
parseType =  (reserved "Int" >> symbol "[" >> symbol "]" >> return IntArrayType)
         <|> (reserved "Int" >> return IntType)
         <|> (reserved "Bool" >> return BoolType)
         <|> (reserved "String" >> return StringType)
         <|> liftM ClassType ident

stmt :: Parser Stmt
stmt =  parseBlock
    <|> parseIf
    -- <|> parseWhile
    <|> parsePrintln
    <|> parseAssign
    <|> parseArrayAssign

parseBlock :: Parser Stmt
parseBlock = do
  stmt <- braces (many stmt)
  return $ Block stmt

parseIf :: Parser Stmt
parseIf = do
  reserved "if"
  cond <- parens expr
  thn <- stmt
  els <- optionMaybe (reserved "else" >> stmt)
  return $ If cond thn els

parsePrintln :: Parser Stmt
parsePrintln = do
  reserved "println"
  t <- parens expr
  semi
  return $ Println t

parseAssign :: Parser Stmt
parseAssign = do
  id <- ident
  symbol "="
  val <- expr
  semi
  return $ Assign id val

parseArrayAssign :: Parser Stmt
parseArrayAssign = do
  id <- ident
  idx <- brackets expr
  symbol "="
  val <- expr
  semi
  return $ ArrayAssign id idx val

expr :: Parser Expr
expr =  (reserved "true"  >> return TrueLit)
    <|> (reserved "false" >> return FalseLit)
    <|> (reserved "this"  >> return This)
    <|> parens expr
    <|> liftM Identifier ident

ident :: Parser Ident
ident = liftM Ident identifier


