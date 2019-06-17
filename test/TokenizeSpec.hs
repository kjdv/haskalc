module TokenizeSpec where

import Test.Hspec
import Control.Exception (evaluate)
import Tokenize

tokenizeSpec :: Spec
tokenizeSpec = do
  describe "tokenize" $ do
    it "recongnizes the math operators" $ do
      tokenize "+-*/^" `shouldBe` [Plus,Minus,Times,Divide,Power]

    it "recognizes parenthesis" $ do
      tokenize "(,)" `shouldBe` [Open,Comma,Close]

    it "recognizes identifiers" $ do
      tokenize "a" `shouldBe` [Identifier "a"]
      tokenize "abc" `shouldBe` [Identifier "abc"]

    it "recognizes numbers" $ do
      tokenize "1" `shouldBe` [Number 1.0]
      tokenize "3.14" `shouldBe` [Number 3.14]
      tokenize "1e2" `shouldBe` [Number 100.0]

    it "splits tokens" $ do
      tokenize "+ -" `shouldBe` [Plus,Minus]
      tokenize "1abc2 def ^" `shouldBe` [Number 1.0, Identifier "abc", Number 2.0, Identifier "def", Power]

    it "fails on bad tokens" $ do
      evaluate (tokenize "£") `shouldThrow` errorCall "unrecognized token '£'"
