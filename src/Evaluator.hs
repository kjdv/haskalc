module Evaluator where

import Parser
import qualified Data.Map as M
import Data.Maybe

data Result = Num Double | Func String ([Result] -> Result) | Err String

instance Show Result where
  show (Num n) = show n
  show (Err e) = "Error: " ++ e
  show (Func name f) = name

instance Eq Result where
  (==) (Num a) (Num b) = a == b
  (==) (Err a) (Err b) = a == b
  (==) (Func a _) (Func b _) = a == b

data Context = Context {
  globals :: M.Map String Result,
  locals :: M.Map String Result
}

getVar :: Context -> String -> Result
getVar ctx key = case M.lookup key (locals ctx) of
  Just r -> r
  Nothing -> case M.lookup key (globals ctx) of
    Just r -> r
    Nothing -> Err ("'" ++ key ++ "': no such variable")

applyU :: (Double -> Double) -> Result -> Result
applyU f (Err s) = Err s
applyU f (Func _ _) = Err "Can't apply to function"
applyU f (Num n) = Num (f n)

applyB :: (Double -> Double -> Double) -> Result -> Result -> Result
applyB f (Num x) (Num y) = Num (f x y)
applyB f (Err s) _ = Err s
applyB f _ (Err s) = Err s
applyB f (Func _ _) _ = Err "Can't apply to function"
apppyB f x (Func n fn) = applyB f (Func n fn) x

class Evaluator a where
  evaluate :: a -> Context -> Result

instance Evaluator Function where
  evaluate (Function name args) ctx =
    case getVar ctx name of
      (Func _ f) -> let eargs = fmap (\x -> evaluate x ctx) args
                     in f eargs
      (Err e) -> Err e
      (Num _) -> Err ("'" ++ name ++ "': not a function")

instance Evaluator Variable where
  evaluate (NumberVar n) _ = Num n
  evaluate (IdentifierVar s) ctx = getVar ctx s
  evaluate (FunctionVar f) ctx = evaluate f ctx

instance Evaluator Factor where
  evaluate (UFactor u f) ctx = applyU (o u) (evaluate f ctx) where
    o UMinusOp = (0-)
  evaluate (VarFactor v) ctx = evaluate v ctx
  evaluate (ExpFactor e) ctx = evaluate e ctx

instance Evaluator PTerm where
  evaluate (PTerm f fs) ctx = foldl combine (evaluate f ctx) fs where
    combine :: Result -> (Binop, Factor) -> Result
    combine acc (o, f) = let fr = evaluate f ctx
                          in applyB (op o) acc fr
    op :: Binop -> (Double -> Double -> Double)
    op PowerOp = (**)

instance Evaluator Term where
  evaluate (Term p ps) ctx = foldl combine (evaluate p ctx) ps where
    combine :: Result -> (Binop, PTerm) -> Result
    combine acc (o, p) = let pr = evaluate p ctx
                          in applyB (op o) acc pr
    op :: Binop -> (Double -> Double -> Double)
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
  evaluate _ _ = Err "todo"

evaluateAndSet :: Statement -> Context -> (Result, Context)
evaluateAndSet (EStatement e) ctx = (evaluate e ctx, ctx)
evaluateAndSet (AStatement (Assignment (VariableDecl name) expr)) ctx = do
  let v = evaluate expr ctx
  let c = Context {
    globals=M.insert name v (globals ctx),
    locals=locals ctx
  }
  (v,  c)
evaluateAndSet (AStatement (Assignment (FunctionDecl name args) expr)) ctx = do
  let f = makeFunc name args expr
  let c = Context {
    globals=M.insert name f (globals ctx),
    locals=locals ctx
  }
  (f, c)
  where
    makeFunc :: String -> [String] -> Expression -> Result
    makeFunc name args expr = Err("implement me")
