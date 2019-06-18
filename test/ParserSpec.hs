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
