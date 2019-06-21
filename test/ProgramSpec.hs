module ProgramSpec where

import Test.Hspec
import Program
import Evaluator (Result(Num, Func, Err))

programSpec :: Spec
programSpec = do
  describe "simple arithmetic" $ do
    let srun = \s -> run defaultContext s
    it "gives nothing on the empty string" $ do
      srun "" `shouldBe` Nothing
      srun "  " `shouldBe` Nothing
    it "prints an error on invalid statements" $ do
      srun "2a" `shouldBe` Just (Err ("no valid statement"))
    it "prints out simple numbers" $ do
      srun "2" `shouldBe` Just (Num 2.0)
    it "does simple binary operations" $ do
      srun "1 + 2" `shouldBe` Just (Num 3.0)
      srun "3 - 4" `shouldBe` Just (Num (-1.0))
      srun "5 * 6" `shouldBe` Just (Num 30)
      srun "7 / 8" `shouldBe` Just (Num 0.875)
      srun "10 ^ 9" `shouldBe` Just (Num 1.0e9)
    it "takes operator presedence into account" $ do
      srun "1 + 2 * 3" `shouldBe` Just (Num 7.0)
      srun "1 * 2 + 3" `shouldBe` Just (Num 5.0)
    it "binary operators go left to right" $ do
      srun "2 - 3 + 4" `shouldBe` Just (Num 3.0)
    it "honours parenthesis" $ do
      srun "(1 + 2) * 3" `shouldBe` Just (Num 9.0)
      srun "2 - (3 + 4)" `shouldBe` Just (Num (-5.0))
    it "supports unary operators" $ do
      srun "-1" `shouldBe` Just (Num (-1))
      srun "--1" `shouldBe` Just (Num 1)
      srun "-(2+3)" `shouldBe` Just (Num (-5))
      srun "-2 * -3" `shouldBe` Just (Num 6)
