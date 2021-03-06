module ProgramSpec where

import Test.Hspec
import Program
import Evaluator (Result(Num, Func, Err))

strip :: String -> String
strip "" = ""
strip (' ':ss) = ss
strip (s:ss) = strip ss

-- run a series of statmentes
mrun :: [String] -> [String]
mrun input = doRun program input where
  doRun :: Program -> [String] -> [String]
  doRun _ [] = []
  doRun p (s:ss) = let (o, p') = run p s in strip o : doRun p' ss

programSpec :: Spec
programSpec = do
  describe "statement execution" $ do
    let srun = \s -> let (o, _) = run program s in strip o
    describe "anomoly reporting" $ do
      it "gives nothing on the empty string" $ do
        srun "" `shouldBe` ""
        srun "  " `shouldBe` ""
      it "prints an error on invalid statements" $ do
        srun "2a" `shouldBe` "Error: no valid statement"
      it "prints an error on invalid tokens" $ do
        srun "%" `shouldBe` "Error: invalid tokens"
      it "prints out simple numbers" $ do
        srun "2" `shouldBe` show 2.0
    describe "simple arithmetic" $ do
      it "does simple binary operations" $ do
        srun "1 + 2" `shouldBe` show 3.0
        srun "3 - 4" `shouldBe` show (-1.0)
        srun "5 * 6" `shouldBe` show 30.0
        srun "7 / 8" `shouldBe` show 0.875
        srun "10 ^ 9" `shouldBe` show 1.0e9
      it "takes operator presedence into account" $ do
        srun "1 + 2 * 3" `shouldBe` show 7.0
        srun "1 * 2 + 3" `shouldBe` show 5.0
      it "binary operators go left to right" $ do
        srun "2 - 3 + 4" `shouldBe` show 3.0
      it "honours parenthesis" $ do
        srun "(1 + 2) * 3" `shouldBe` show 9.0
        srun "2 - (3 + 4)" `shouldBe` show (-5.0)
      it "supports unary operators" $ do
        srun "-1" `shouldBe` show (-1.0)
        srun "--1" `shouldBe` show 1.0
        srun "-(2+3)" `shouldBe` show (-5.0)
        srun "-2 * -3" `shouldBe` show 6.0
      it "does power before multiplications" $ do
        srun "(2 * 10) ^ 2" `shouldBe` show 400.0
        srun "2 * 10 ^ 2" `shouldBe` show 200.0
    describe "functions" $ do
      it "has function calls" $ do
        srun "exp (2)" `shouldBe` show (exp 2)
      it "checks the number of arguments" $ do
        srun "exp ()" `shouldBe` "Error: function takes exactly 1 argument"
        srun "exp (1, 2)" `shouldBe` "Error: function takes exactly 1 argument"
  describe "stateful program execution" $ do
    describe "variable assignments" $ do
      it "allows assigning a variable" $ do
        mrun ["a=2", "a+1"] `shouldBe` ["2.0", "3.0"]
      it "allows overriding existing variables" $ do
        mrun ["pi", "pi=3", "pi"] `shouldBe` [show pi, "3.0", "3.0"]
        mrun ["sin", "sin=1", "sin"] `shouldBe` ["sin(x)", "1.0", "1.0"]
      it "allows creating functions" $ do
        mrun ["f()=1.0", "f()"] `shouldBe` ["f()", "1.0"]
      it "binds local variables to functions" $ do
        mrun ["f(x,y) = x * y", "f(2, 3)"] `shouldBe` ["f(x,y)", "6.0"]
      it "can use global variables in functions" $ do
        mrun ["f(x) = x + y", "y=1", "f(2)"] `shouldBe` ["f(x)", "1.0", "3.0"]
      it "checks the number of arguments" $ do
        mrun ["f()=1.0", "f(1)"] `shouldBe` ["f()", "Error: function 'f' takes 0 argument(s), 1 provided"]
      it "binds the last result to 'ans'" $ do
        mrun ["2.0", "ans", "ans*3"] `shouldBe` ["2.0", "2.0", "6.0"]
      it "binds the result to a 'ans_N', N being the statement number" $ do
        mrun ["2.0", "3.0", "ans_1 * ans_2", "ans_3"] `shouldBe` ["2.0", "3.0", "6.0", "6.0"]
