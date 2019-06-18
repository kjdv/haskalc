module ParserSpec where

import Test.Hspec
import Tokenizer
import Parser

parserSpec :: Spec
parserSpec = do
  describe "parser fundamentals" $ do
    describe "item" $ do
      it "parsers a single token" $ do
        parse item [Plus, Minus] `shouldBe` Just (Plus, [Minus])
      it "returns emtpy on empty input" $ do
        parse item [] `shouldBe` Nothing

    describe "symbol" $ do
      let p = symbol Plus
      it "matches a specific token" $ do
        parse p [Plus, Minus] `shouldBe` Just (Plus, [Minus])
      it "fails to match anything else" $ do
        parse p [] `shouldBe` Nothing
        parse p [Minus] `shouldBe` Nothing
