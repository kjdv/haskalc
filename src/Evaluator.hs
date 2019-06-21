module Evaluator where

import Parser
import Data.Map (Map, lookup)
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
  e Nothing = Error ("'" ++ key ++ "': no such variable")
  e (Just(Constant r)) = r
  e _ = Error ("'" ++ key ++ "': not a variable")

evalFunc :: Context -> String -> [Result] -> Result
evalFunc ctx key args = e (getVar ctx key) where
  e Nothing = Error ("'" ++ key ++ "': no such variable")
  e (Just(Constant _)) = Error ("'" ++ key ++ "': not a function")
  e (Just(Callable fn)) = fn args

data Result = Number Double | Error String deriving(Show, Eq)

applyU :: (Double -> Double) -> Result -> Result
applyU f (Error s) = Error s
applyU f (Number n) = Number (f n)

applyB :: (Double -> Double -> Double) -> Result -> Result -> Result
applyB f (Error s) _ = Error s
applyB f _ (Error s) = Error s
applyB f (Number x) (Number y) = Number (f x y)

class Evaluator a where
  evaluate :: a -> Context -> Result

instance Evaluator Function where
  evaluate (Function name args) ctx =
    let eargs = fmap (\x -> evaluate x ctx) args
    in evalFunc ctx name eargs

instance Evaluator Variable where
  evaluate (NumberVar n) _ = Number n
  evaluate (IdentifierVar s) ctx = evalVar ctx s
  evaluate (FunctionVar f) ctx = evaluate f ctx

instance Evaluator Factor where
  evaluate (UFactor u f) ctx = applyU (o u) (evaluate f ctx) where
    o UMinusOp = (0-)
  evaluate (VarFactor v) ctx = evaluate v ctx
  evaluate (ExpFactor e) ctx = evaluate e ctx

instance Evaluator Term where
  evaluate (Term f fs) ctx = foldl combine (evaluate f ctx) fs where
    combine :: Result -> (Binop, Factor) -> Result
    combine acc (o, f) = let fr = evaluate f ctx
                          in applyB (op o) acc fr
    op :: Binop -> (Double -> Double -> Double)
    op PowerOp = (**)
    op TimesOp = (*)
    op DivideOp = (/)

instance Evaluator Expression where
  evaluate (Expression t ts) ctx = foldl combine (evaluate t ctx) ts where
    combine :: Result -> (Binop, Term) -> Result
    combine acc (o, t) = let tr = evaluate t ctx
                          in applyB (op o) acc tr
    op :: Binop -> (Double -> Double -> Double)
    op PlusOp = (+)
    op MinusOp = (-)

instance Evaluator Statement where
  evaluate (EStatement e) ctx = evaluate e ctx
  evaluate _ _ = Error "todo"
