module TokenizerSpec where

import Test.Hspec
import Control.Exception (evaluate)
import Tokenizer

tokenizerSpec :: Spec
tokenizerSpec = do
  describe "tokenize" $ do
    it "recongnizes the math operators" $ do
      tokenize "+-*/^=" `shouldBe` [Plus,Minus,Times,Divide,Power,Equals]

    it "recognizes parenthesis" $ do
      tokenize "(,)" `shouldBe` [Open,Comma,Close]

    it "recognizes identifiers" $ do
      tokenize "a" `shouldBe` [Identifier "a"]
      tokenize "abc" `shouldBe` [Identifier "abc"]
      tokenize "ab_c" `shouldBe` [Identifier "ab_c"]
      tokenize "a1" `shouldBe` [Identifier "a1"]
      tokenize "1a" `shouldBe` [Number 1.0, Identifier "a"]

    it "recognizes numbers" $ do
      tokenize "1" `shouldBe` [Number 1.0]
      tokenize "3.14" `shouldBe` [Number 3.14]
      tokenize "1e2" `shouldBe` [Number 100.0]

    it "splits tokens" $ do
      tokenize "+ -" `shouldBe` [Plus,Minus]
      tokenize "1abc2+ def ^" `shouldBe` [Number 1.0, Identifier "abc2", Plus, Identifier "def", Power]

    it "fails on bad tokens" $ do
      evaluate (tokenize "£") `shouldThrow` errorCall "unrecognized token '£'"
