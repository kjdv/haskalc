module ProgramSpec where

import Test.Hspec
import Program
import Evaluator (Result(Number, Error))

programSpec :: Spec
programSpec = do
  describe "simple arithmetic" $ do
    let srun = \s -> run defaultContext s
    it "prints out simple numbers" $ do
      srun "2" `shouldBe` Just (Number 2.0)
    it "does simple binary operations" $ do
      srun "1 + 2" `shouldBe` Just (Number 3.0)
      srun "3 - 4" `shouldBe` Just (Number (-1.0))
      srun "5 * 6" `shouldBe` Just (Number 30)
      srun "7 / 8" `shouldBe` Just (Number 0.875)
      srun "10 ^ 9" `shouldBe` Just (Number 1.0e9)
