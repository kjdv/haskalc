module Program where

import Evaluator
import Tokenizer (tokenize)
import Parser (parse, parseStatement)
import Data.Map (fromList)

defaultContext = Context {
  globals=fromList [("b", Constant (Number 4.0)), ("shadow", Constant (Number 1.0))],
  locals=fromList  [("a", Constant (Number 2.0)), ("shadow", Constant (Number 3.0))]
}

run :: Context -> String -> Maybe Result
run ctx s = do
  let toks = tokenize s
  (stat,_) <- parse parseStatement toks
  Just (evaluate stat ctx)

display :: Maybe Result -> String
display Nothing = ""
display (Just (Number n)) = show n
display (Just (Error e)) = "Error: " ++ e

runSingle :: String -> String
runSingle s = display (run defaultContext s)
