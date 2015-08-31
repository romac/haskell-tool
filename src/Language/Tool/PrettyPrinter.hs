
{-# LANGUAGE OverloadedStrings
           , FlexibleInstances #-}

module Language.Tool.PrettyPrinter
(
  prettyPrint
)
where

import Prelude           hiding (id)

import Text.PrettyPrint

import Language.Tool.AST

prettyPrint :: Pretty a => a -> String
prettyPrint x = render (pretty x)

infixl 4 ~>
(~>) :: Doc -> Doc -> Doc
(~>) a b = (a <+> lbrace) $+$ nest 2 b $+$ rbrace

mainMethod :: Doc -> Doc
mainMethod stmts = "def main(): Unit =" ~> stmts

extends :: Maybe Ident -> Doc
extends Nothing   = empty
extends (Just id) = "extends" <+> pretty id

binOp :: String -> Expr -> Expr -> Doc
binOp op a b = pretty a <+> text op <+> pretty b

class Pretty a where
  pretty :: a -> Doc

instance Pretty Program where
  pretty (Program main classes) = pretty main $+$ pretty classes

instance Pretty MainObject where
  pretty (MainObject id stmts) =
    "object" <+> pretty id
      ~> mainMethod (pretty stmts)

instance Pretty ClassDecl where
  pretty (ClassDecl id parent vars meths) =
    "class" <+> pretty id <+> extends parent
      ~> pretty vars $$ pretty meths

instance Pretty VarDecl where
  pretty (VarDecl id tpe) =
    "var" <+> pretty id <> ":" <+> pretty tpe <> semi

instance Pretty MethodDecl where
  pretty (MethodDecl id tpe args vars stmts retExpr) =
    text "def" <+> pretty id <> parens argsDoc <> ":" <+> pretty tpe <+> "="
      ~> pretty vars $$ pretty stmts $$ "return" <+> pretty retExpr <> ";"
    where
      argsDoc = hsep (punctuate (char ',') (fmap pretty args))

instance Pretty Formal where
  pretty (Formal id tpe) =
    pretty id <> ":" <+> pretty tpe

instance Pretty Type where
  pretty IntArrayType   = text "Int[]"
  pretty IntType        = text "Int"
  pretty BoolType       = text "Bool"
  pretty StringType     = text "String"
  pretty (ClassType id) = pretty id

instance Pretty Stmt where

  -- FIXME: wrap in braces
  pretty (Block ss) =
    pretty ss

  pretty (If cond thn Nothing) =
    "if" <+> parens (pretty cond)
      ~> pretty thn

  pretty (If cond thn (Just els)) =
    pretty (If cond thn Nothing) <+> "else"
      ~> pretty els

  pretty (While cond stmt) =
    "while" <+> parens (pretty cond) ~> pretty stmt

  pretty (Println val) =
    "println" <> parens (pretty val) <> ";"

  pretty (Assign id val) =
    pretty id <+> "=" <+> pretty val <> ";"

  pretty (ArrayAssign arr idx val) =
    pretty arr <> brackets (pretty idx) <+> "=" <+> pretty val <> ";"

instance Pretty Expr where
  pretty (And a b)      = binOp "&&" a b
  pretty (Or a b)       = binOp "||" a b
  pretty (Plus a b)     = binOp "+"  a b
  pretty (Minus a b)    = binOp "-"  a b
  pretty (Times a b)    = binOp "*"  a b
  pretty (Div a b)      = binOp "/"  a b
  pretty (LessThan a b) = binOp "<"  a b
  pretty (Equals a b)   = binOp "==" a b

  pretty (ArrayRead arr idx) =
    pretty arr <> brackets (pretty idx)

  pretty (ArrayLength arr) =
    pretty arr <> ".length"

  pretty (MethodCall obj meth args) =
    pretty obj <> "." <> pretty meth <> parens argsDoc
    where
      argsDoc = hsep (punctuate (char ',') (fmap pretty args))

  pretty (IntLit val)       = integer val
  pretty (StringLit str)    = "\"" <> text str <> "\""
  pretty (TrueLit)          = text "true"
  pretty (FalseLit)         = text "false"
  pretty (Identifier id)    = pretty id
  pretty (This)             = text "this"
  pretty (NewIntArray size) = "new Int" <> brackets (pretty size)
  pretty (New id)           = "new" <+> pretty id <> parens empty
  pretty (Not expr)         = "!" <> parens (pretty expr)

instance Pretty Ident where
  pretty (Ident i) = text i

instance Pretty a => Pretty [a] where
  pretty = vcat . map pretty

