module TokenizerSpec where

import Test.Hspec
import Tokenizer

tokenizerSpec :: Spec
tokenizerSpec = do
  describe "tokenize" $ do
    it "recongnizes the math operators" $ do
      tokenize "+-*/^=" `shouldBe` Just [Plus,Minus,Times,Divide,Power,Equals]

    it "recognizes parenthesis" $ do
      tokenize "(,)" `shouldBe` Just [Open,Comma,Close]

    it "recognizes identifiers" $ do
      tokenize "a" `shouldBe` Just [Identifier "a"]
      tokenize "abc" `shouldBe` Just [Identifier "abc"]
      tokenize "ab_c" `shouldBe` Just [Identifier "ab_c"]
      tokenize "a1" `shouldBe` Just [Identifier "a1"]
      tokenize "1a" `shouldBe` Just [Number 1.0, Identifier "a"]

    it "recognizes numbers" $ do
      tokenize "1" `shouldBe` Just [Number 1.0]
      tokenize "3.14" `shouldBe` Just [Number 3.14]
      tokenize "1e2" `shouldBe` Just [Number 100.0]

    it "splits tokens" $ do
      tokenize "+ -" `shouldBe` Just [Plus,Minus]
      tokenize "1abc2+ def ^" `shouldBe` Just [Number 1.0, Identifier "abc2", Plus, Identifier "def", Power]

    it "fails on bad tokens" $ do
      tokenize "£" `shouldBe` Nothing
