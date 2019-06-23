module Program where

import Evaluator
import Tokenizer (tokenize)
import Parser (parse, parseStatement)
import Data.Map (fromList)

wrap1 :: (Double -> Double ) -> ([Result] -> Result)
wrap1 f = \args -> case checked args of
  x -> applyU f x
  where
    checked [x] = x
    checked _ = Err ("function takes exactly 1 argument")

defaultContext = Context {
  globals=fromList [
    ("e", Num (exp 1)),
    ("pi", Num pi),
    ("exp", Func "exp(x)" (wrap1 exp)),
    ("sqrt", Func "sqrt(x)" (wrap1 sqrt)),
    ("log", Func "log(x)" (wrap1 log)),
    ("log10", Func "log10(x)" (wrap1 (logBase 10))),
    ("log2", Func "log2(x)" (wrap1 (logBase 2))),
    ("sin", Func "sin(x)" (wrap1 sin)),
    ("cos", Func "cos(x)" (wrap1 cos)),
    ("tan", Func "tan(x)" (wrap1 tan)),
    ("asin", Func "asin(x)" (wrap1 asin)),
    ("acos", Func "acos(x)" (wrap1 acos)),
    ("atan", Func "atan(x)" (wrap1 atan)),
    ("sinh", Func "sinh(x)" (wrap1 sinh)),
    ("cosh", Func "cosh(x)" (wrap1 cosh)),
    ("tanh", Func "tanh(x)" (wrap1 tanh)),
    ("asinh", Func "asinh(x)" (wrap1 asinh)),
    ("acosh", Func "acosh(x)" (wrap1 acosh)),
    ("atanh", Func "atanh(x)" (wrap1 atanh))
  ],
  locals=fromList  []
}

run :: Context -> String -> Maybe Result
run ctx s = do
  case tokenize s of
    [] -> Nothing
    toks -> case parse parseStatement toks of
      Nothing -> Just (Err "no valid statement")
      Just (stat, _) -> Just (evaluate stat ctx)

runSingle :: String -> String
runSingle s = case (run defaultContext s) of
  Nothing -> ""
  Just r -> show r

runStep :: Int -> String -> (String, Int)
runStep ctx input = case run defaultContext input of
  Nothing -> (show ctx ++ ": ", ctx+1)
  Just r -> (show ctx ++ ": " ++ show r, ctx +1)

newtype Program = Program (String -> (String, Program))

program :: Program
program = Program (step 1) where
  step :: Int -> String -> (String, Program)
  step ctx input = let (s, c) = runStep ctx input
                    in (s, Program (step c))

runProgram :: Program -> String -> (String, Program)
runProgram (Program p) input = p input
