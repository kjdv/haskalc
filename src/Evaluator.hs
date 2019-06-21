module Evaluator where

import Parser
import Data.Map
import Data.Maybe

data ContextVar = Constant Result | Callable ([Result] -> Result)

data Context = Context {
  globals :: Map String ContextVar,
  locals :: Map String ContextVar
}

getVar :: Context -> String -> Maybe ContextVar
getVar ctx key = (listToMaybe . Prelude.take 1 . catMaybes) [Data.Map.lookup key (locals ctx), Data.Map.lookup key (globals ctx)]

evalVar :: Context -> String -> Result
evalVar ctx key = e (getVar ctx key) where
  e Nothing = Error "no such variable"
  e (Just(Constant r)) = r
  e _ = error "todo: implement me"

data Result = Empty | Number Double | Error String deriving(Show, Eq)

class Evaluator a where
  evaluate :: a -> Context -> Result

instance Evaluator Variable where
  evaluate (NumberVar n) _ = Number n
  evaluate (IdentifierVar s) ctx = evalVar ctx s
  evaluate v _ = Error ("evaluator not implemented yet")
