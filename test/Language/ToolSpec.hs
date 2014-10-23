module Language.ToolSpec
(
    main
  , spec
)
where

import Control.Applicative ((<$>), (<*>), pure)

import Test.Hspec
import Test.QuickCheck

import Language.Tool.AST
import Language.Tool.Parser (parseString)
import Language.Tool.PrettyPrinter (prettyPrint)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tool compiler" $ do
    it "pretty(prog) == pretty(parse(pretty(prog)))" $
      property prop_prettyParse

prop_prettyParse prog =
  pretty prog == (pretty . parse . pretty) prog

pretty :: Program -> String
pretty = prettyPrint

parse :: String -> Program
parse = parseString

instance Arbitrary Program where
  arbitrary = Program <$> arbitrary
                      <*> arbitrary

instance Arbitrary MainObject where
  arbitrary = MainObject <$> arbitrary
                         <*> arbitrary

instance Arbitrary ClassDecl where
  arbitrary = ClassDecl <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance Arbitrary VarDecl where
  arbitrary = VarDecl <$> arbitrary
                      <*> arbitrary

instance Arbitrary MethodDecl where
  arbitrary = MethodDecl <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

instance Arbitrary Formal where
  arbitrary = Formal <$> arbitrary
                     <*> arbitrary

instance Arbitrary Type where
  arbitrary = elements [ IntArrayType, IntType, BoolType, StringType ]

instance Arbitrary Stmt where
  arbitrary = oneof [
      If <$> arbitrary <*> arbitrary <*> arbitrary
    , While <$> arbitrary <*> arbitrary
    , Println <$> arbitrary
    -- , Assign <$> arbitrary <*> arbitrary
    -- , ArrayAssign <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Expr where
  arbitrary = oneof [
      pure This
    , pure TrueLit
    , pure FalseLit
    , MethodCall <$> arbitrary <*> arbitrary <*> pure []
    , New <$> arbitrary
    , NewIntArray <$> arbitrary
    -- , IntLit <$> arbitrary
    , Not <$> arbitrary
    ]

instance Arbitrary Ident where
  arbitrary = Ident <$> identifier

identifier :: Gen String
identifier = (:) <$> letter <*> listOf alphaNum

letter :: Gen Char
letter = elements (['A'..'Z'] ++ ['a' .. 'z'])

digit :: Gen Char
digit = elements ['0' .. '9']

alphaNum :: Gen Char
alphaNum = oneof [letter, digit]

