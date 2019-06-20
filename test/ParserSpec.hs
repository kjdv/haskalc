module ParserSpec where

import Test.Hspec
import Tokenizer
import Parser

parserSpec :: Spec
parserSpec = do
  describe "parser fundamentals" $ do
    describe "end" $ do
      it "matches the empty list" $ do
        parse end [] `shouldBe` Just ((), [])
      it "does not match the non-empty list" $ do
        parse end [Plus] `shouldBe` Nothing

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

    describe "choice" $ do
      let p = (symbol Plus) `choice` (symbol Minus) `choice` (symbol Times)
      it "matches either choice" $ do
        parse p [Plus, Minus] `shouldBe` Just (Plus, [Minus])
        parse p [Minus, Plus] `shouldBe` Just (Minus, [Plus])
        parse p [Times] `shouldBe` Just (Times, [])
      it "fails to match others" $ do
        parse p [Divide] `shouldBe` Nothing
      it "matches the first" $ do
        let q = (transform (symbol Plus) (\_ -> Minus)) `choice`
                (transform (symbol Times) (\_ -> Divide)) `choice`
                (symbol Plus) `choice`
                (symbol Times) `choice`
                (symbol Divide)
        parse q [Plus] `shouldBe` Just (Minus, []) -- choice that turs a plus into a minus picked first
        parse q [Times] `shouldBe` Just (Divide, [])
        parse q [Divide] `shouldBe` Just (Divide, [])

    describe "zeroOrMore" $ do
      let p = zeroOrMore (symbol Plus)
      it "matches zero instances" $ do
        parse p [] `shouldBe` Just ([], [])
        parse p [Minus] `shouldBe` Just ([], [Minus])
      it "matches a single instance" $ do
        parse p [Plus] `shouldBe` Just ([Plus], [])
        parse p [Plus, Minus] `shouldBe` Just ([Plus], [Minus])
      it "matches n>1 instances" $ do
        parse p [Plus, Plus] `shouldBe` Just ([Plus,Plus], [])
        parse p [Plus, Plus, Plus, Minus] `shouldBe` Just ([Plus, Plus, Plus], [Minus])

    describe "oneOrMore" $ do
      let p = oneOrMore (symbol Plus)
      it "does not match zero instances" $ do
        parse p [] `shouldBe` Nothing
        parse p [Minus] `shouldBe` Nothing
      it "matches a single instance" $ do
        parse p [Plus] `shouldBe` Just ([Plus], [])
        parse p [Plus, Minus] `shouldBe` Just ([Plus], [Minus])
      it "matches n>1 instances" $ do
        parse p [Plus, Plus] `shouldBe` Just ([Plus,Plus], [])
        parse p [Plus, Plus, Plus, Minus] `shouldBe` Just ([Plus, Plus, Plus], [Minus])
