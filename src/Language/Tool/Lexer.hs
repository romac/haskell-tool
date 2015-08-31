
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.Tool.Lexer
(
    toolDef
  , lexer
  , identifier
  , reserved
  , reservedOp
  , parens
  , integer
  , stringLiteral
  , semi
  , colon
  , whiteSpace
  , braces
  , brackets
  , symbol
  , dot
  , comma
)
where

import           Text.Parsec.Language
import qualified Text.Parsec.Token    as T

toolDef :: LanguageDef st
toolDef =
  javaStyle { T.nestedComments  = False
            , T.reservedNames   = [ "object", "class", "def", "var"
                                , "Unit", "main", "String", "extends"
                                , "Int", "Bool", "while", "if", "else"
                                , "return", "length", "true", "false"
                                , "this", "new", "println" ]
            , T.reservedOpNames = [ "+", "-", "*", "/"
                                , "<", "||", "&&", "!" ]
            , T.caseSensitive   = True
            }

lexer = T.makeTokenParser toolDef

identifier    = T.identifier    lexer
reserved      = T.reserved      lexer
reservedOp    = T.reservedOp    lexer
parens        = T.parens        lexer
integer       = T.integer       lexer
stringLiteral = T.stringLiteral lexer
semi          = T.semi          lexer
colon         = T.colon         lexer
whiteSpace    = T.whiteSpace    lexer
braces        = T.braces        lexer
brackets      = T.brackets      lexer
symbol        = T.symbol        lexer
dot           = T.dot           lexer
comma         = symbol ","

