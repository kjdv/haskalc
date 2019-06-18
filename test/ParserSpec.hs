module ParserSpec where

import Test.Hspec
import Tokenizer
import Parser

parserSpec :: Spec
parserSpec = do
  describe "parseVariable" $ do
    it "parses numbers as variable" $ do
      parseVariable [Number 1.0] `shouldBe` Just (VariableNum 1.0, [])

    it "parses identifiers as variable" $ do
      parseVariable [Identifier "abc"] `shouldBe` Just (VariableVar "abc", [])

    it "parses a function call as variable" $ do
      parseVariable [Identifier "a", Open, Close] `shouldBe` Just (VariableFunc (Function []), [])

  describe "parseFunction" $ do
    it "parses a function without arguments" $ do
      parseFunction [Identifier "a", Open, Close] `shouldBe` Just (Function [], [])

  describe "parseFactor" $ do
    it "parses a variable as factor" $ do
      parseFactor [Number 1.0] `shouldBe` Just (FactorVar (VariableNum 1.0), [])

  describe "parseUFactor" $ do
    it "parses a normal factor" $ do
      parseUFactor [Number 1.0] `shouldBe` Just (UFactor Nothing (FactorVar (VariableNum 1.0)), [])
    it "parses a negated factor" $ do
      parseUFactor [Minus, Number 1.0] `shouldBe` Just (UFactor (Just UnMinusOp) (FactorVar (VariableNum 1.0)), [])

  describe "parseTerm" $ do
    it "parses an identifier as term" $ do
      parseTerm [Identifier "a"] `shouldBe` Just (Term (UFactor Nothing (FactorVar (VariableVar "a"))) [], [])
    --it "combines with operators" $ do
    --  parseTerm [Identifier "a", Times, Number 1.0] `shouldBe`
    --    Just (Term (UFactor Nothing (FactorVar (VariableVar "a")))
    --      [(TimesOp, UFactor Nothing (FactorVar (VariableNum 1.0)))],
    --    [])
