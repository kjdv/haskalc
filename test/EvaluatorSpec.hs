module EvaluatorSpec where

import Test.Hspec
import Evaluator
import Parser
import Data.Map

evaluatorSpec :: Spec
evaluatorSpec = do
  describe "evaluator" $ do
    let ctx = Context {
      globals=fromList [("b", Num 4.0),
                        ("shadow", Num 1.0)],
      locals=fromList  [("a", Num 2.0),
                        ("shadow", Num 3.0),
                        ("sum", Func "sum(...)" (\xs -> Prelude.foldl (applyB (+)) (Num 0) xs))]
    }
    it "evaluates plain numbers" $ do
      evaluate (NumberVar 1.0) ctx `shouldBe` Num 1.0
    it "retrieves variables" $ do
      evaluate (IdentifierVar "a") ctx `shouldBe` Num 2.0
      evaluate (IdentifierVar "b") ctx `shouldBe` Num 4.0
      evaluate (IdentifierVar "shadow") ctx `shouldBe` Num 3.0
    it "evaluates functions" $ do
      let e1 = Expression (Term (VarFactor (NumberVar 1.0)) []) []
      let e2 = Expression (Term (VarFactor (NumberVar 2.0)) []) []
      evaluate (FunctionVar (Function "sum" [e1, e2])) ctx `shouldBe` Num 3.0
