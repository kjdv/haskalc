module EvaluatorSpec where

import Test.Hspec
import Evaluator
import Parser
import Data.Map

evaluatorSpec :: Spec
evaluatorSpec = do
  describe "evaluator" $ do
    let ctx = Context {
      globals=fromList [("b", Constant (Number 4.0)),
                        ("shadow", Constant (Number 1.0))],
      locals=fromList  [("a", Constant (Number 2.0)),
                        ("shadow", Constant (Number 3.0)),
                        ("sum", Callable (\xs -> Prelude.foldl (applyB (+)) (Number 0) xs ))]
    }
    it "evaluates plain numbers" $ do
      evaluate (NumberVar 1.0) ctx `shouldBe` Number 1.0
    it "retrieves variables" $ do
      evaluate (IdentifierVar "a") ctx `shouldBe` Number 2.0
      evaluate (IdentifierVar "b") ctx `shouldBe` Number 4.0
      evaluate (IdentifierVar "shadow") ctx `shouldBe` Number 3.0
    it "evaluates functions" $ do
      let e1 = Expression (Term (VarFactor (NumberVar 1.0)) []) []
      let e2 = Expression (Term (VarFactor (NumberVar 2.0)) []) []
      evaluate (FunctionVar (Function "sum" [e1, e2])) ctx `shouldBe` Number 3.0
