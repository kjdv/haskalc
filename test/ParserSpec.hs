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
      it "does not execute the second option when not needed" $ do
        let q = (symbol Plus) `choice` (transform (symbol Plus) (\_ -> error "should not be called"))
        parse q [Plus] `shouldBe` Just (Plus, [])

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

    describe "zeroOrMoreSep" $ do
      let p = zeroOrMoreSep (symbol Plus) (symbol Comma)
      it "matches zero instances" $ do
        parse p [] `shouldBe` Just ([], [])
        parse p [Minus] `shouldBe` Just ([], [Minus])
      it "matches a single instance" $ do
        parse p [Plus] `shouldBe` Just ([Plus], [])
        parse p [Plus, Comma, Minus] `shouldBe` Just ([Plus], [Comma, Minus])
      it "matches n>1 instances" $ do
        parse p [Plus, Comma, Plus] `shouldBe` Just ([Plus,Plus], [])
        parse p [Plus, Comma, Plus, Comma, Plus, Minus] `shouldBe` Just ([Plus, Plus, Plus], [Minus])

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

    describe "oneOrMoreSep" $ do
      let p = oneOrMoreSep (symbol Plus) (symbol Comma)
      it "does not match zero instances" $ do
        parse p [] `shouldBe` Nothing
        parse p [Minus] `shouldBe` Nothing
      it "matches a single instance" $ do
        parse p [Plus] `shouldBe` Just ([Plus], [])
        parse p [Plus, Comma, Minus] `shouldBe` Just ([Plus], [Comma, Minus])
      it "matches n>1 instances" $ do
        parse p [Plus, Comma, Plus] `shouldBe` Just ([Plus,Plus], [])
        parse p [Plus, Comma, Plus, Comma, Plus, Minus] `shouldBe` Just ([Plus, Plus, Plus], [Minus])

  describe "clauses" $ do
    describe "variables" $ do
      it "parses identifiers" $ do
        parse parseVariable [Identifier "x"] `shouldBe` Just (IdentifierVar "x", [])
      it "parses numbers" $ do
        parse parseVariable [Number 1.0] `shouldBe` Just (NumberVar 1.0, [])
      it "parses functions" $ do
        parse parseVariable [Identifier "x", Open, Close] `shouldBe` Just (FunctionVar (Function "x" []), [])
        parse parseVariable [Identifier "x", Open, Identifier "y", Close] `shouldBe` Just (FunctionVar (Function "x" [EString "y"]), [])
        parse parseVariable [Identifier "x", Open, Identifier "y", Comma, Identifier "z", Close] `shouldBe` Just (FunctionVar (Function "x" [EString "y", EString "z"]), [])

    describe "factors" $ do
      it "parses a variable" $ do
        parse parseFactor [Number 1.0] `shouldBe` Just (VarFactor (NumberVar 1.0), [])
      it "parses an expression" $ do
        parse parseFactor [Open, Identifier "x", Close] `shouldBe` Just (ExpFactor (EString "x"), [])
      it "parses a unary operator" $ do
        parse parseFactor [Minus, Number 1.0] `shouldBe` Just (UFactor UMinusOp (VarFactor (NumberVar 1.0)), [])
