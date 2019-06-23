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
        parse parseVariable [Identifier "x", Open, Identifier "y", Close] `shouldBe`
          Just (FunctionVar (Function "x" [Expression (Term (PTerm (VarFactor (IdentifierVar "y")) []) []) []]),[])
        parse parseVariable [Identifier "x", Open, Identifier "y", Comma, Identifier "z", Close] `shouldBe`
          Just (FunctionVar (Function "x" [Expression (Term (PTerm (VarFactor (IdentifierVar "y")) []) []) [],Expression (Term (PTerm (VarFactor (IdentifierVar "z")) []) []) []]),[])

    describe "factors" $ do
      it "parses a variable" $ do
        parse parseFactor [Number 1.0] `shouldBe` Just (VarFactor (NumberVar 1.0), [])
      it "parses an expression" $ do
        parse parseFactor [Open, Identifier "x", Close] `shouldBe`
          Just (ExpFactor (Expression (Term (PTerm (VarFactor (IdentifierVar "x")) []) []) []),[])
      it "parses a unary operator" $ do
        parse parseFactor [Minus, Number 1.0] `shouldBe` Just (UFactor UMinusOp (VarFactor (NumberVar 1.0)), [])

    describe "terms" $ do
      it "parses a variable" $ do
        parse parseTerm [Number 1.0] `shouldBe` Just (Term (PTerm (VarFactor (NumberVar 1.0)) []) [], [])
      it "chains operators" $ do
        parse parseTerm [Number 1.0, Power, Number 2.0, Times, Number 3.0, Divide, Number 4.0] `shouldBe`
          Just (Term (PTerm (VarFactor (NumberVar 1.0)) [(PowerOp,VarFactor (NumberVar 2.0))]) [(TimesOp,PTerm (VarFactor (NumberVar 3.0)) []),(DivideOp,PTerm (VarFactor (NumberVar 4.0)) [])],[])

    describe "expressions" $ do
      it "parses a variable" $ do
        parse parseExpression [Number 1.0] `shouldBe`
          Just (Expression (Term (PTerm (VarFactor (NumberVar 1.0)) []) []) [],[])
      it "chains operators" $ do
        parse parseExpression [Number 1.0, Plus, Number 2.0, Minus, Number 3.0] `shouldBe`
          Just (Expression (Term (PTerm (VarFactor (NumberVar 1.0)) []) []) [
            (PlusOp, Term (PTerm (VarFactor (NumberVar 2.0)) []) []),
            (MinusOp, Term (PTerm (VarFactor (NumberVar 3.0)) []) [])
          ],[])
      it "prioritizes times (and like) over plus (and like)" $ do
        parse parseExpression [Number 1.0, Plus, Number 2.0, Times, Number 3.0] `shouldBe`
          Just (Expression (Term (PTerm (VarFactor (NumberVar 1.0)) []) []) [
            (PlusOp,
              Term (PTerm (VarFactor (NumberVar 2.0)) []) [
              (TimesOp, PTerm (VarFactor (NumberVar 3.0)) [])
            ])
          ],[])
      it "prioritizes parenthized terms" $ do
        parse parseExpression [Open, Number 1.0, Plus, Number 2.0, Close, Times, Number 3.0] `shouldBe`
          Just (Expression (Term (PTerm (ExpFactor
            (Expression
              (Term (PTerm (VarFactor (NumberVar 1.0)) []) []) [
              (PlusOp, Term (PTerm (VarFactor (NumberVar 2.0)) []) [])
            ])) []) [
              (TimesOp, PTerm (VarFactor (NumberVar 3.0)) [])
            ]) [],[])

    describe "statements"  $ do
      it "accepts a single expression" $ do
        parse parseStatement [Number 1.0] `shouldBe`
          Just (EStatement (Expression (Term (PTerm (VarFactor (NumberVar 1.0)) []) []) []),[])
      it "rejects if extra tokens exists" $ do
        parse parseStatement [Number 1.0, Number 2.0] `shouldBe` Nothing

    describe "assignments" $ do
      let expr = [Number 1.0]
      let parsedExpr = Expression (Term (PTerm (VarFactor (NumberVar 1.0)) []) []) []
      it "does variable assignments" $ do
        parse parseStatement ([Identifier "a", Equals] ++ expr) `shouldBe`
          Just (AStatement (Assignment (VariableDecl "a") parsedExpr), [])
      it "does function assignments" $ do
        parse parseStatement ([Identifier "a", Open, Close, Equals] ++ expr) `shouldBe`
          Just (AStatement (Assignment (FunctionDecl "a" []) parsedExpr), [])
        parse parseStatement ([Identifier "a", Open, Identifier "b", Comma, Identifier "c", Close, Equals] ++ expr) `shouldBe`
          Just (AStatement (Assignment (FunctionDecl "a" ["b", "c"]) parsedExpr), [])
