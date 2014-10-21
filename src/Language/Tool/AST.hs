
module Language.Tool.AST
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
  , Ident (..)
)
where

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

