module Program where

import Evaluator
import Tokenizer (tokenize)
import Parser (parse, parseStatement)
import Data.Map (fromList)

defaultContext = Context {
  globals=fromList [("b", Num 4.0), ("shadow", Num 5.0)],
  locals=fromList  [("a", Num 2.0), ("shadow", Num 3.0)]
}

run :: Context -> String -> Maybe Result
run ctx s = do
  let toks = tokenize s
  (stat,_) <- parse parseStatement toks
  Just (evaluate stat ctx)

runSingle :: String -> String
runSingle s = case (run defaultContext s) of
  Nothing -> ""
  Just r -> show r
