module Parser where

import Tokenize

{- Grammar:

program = assignment | expression
assignment = decleration '=' expression
expression = term ('+' term | '-' term)*
term = factor ('^' ufactor | '*' ufactor | '/' ufactor)*
ufactor = ['-'] factor
factor = (variable | '(' expression ')')

variable = function | identifier | number
function = identifier '(' expression (',' expression)* ')'

decleration = function_decleration | variable_decleration
function_decleration = identifier '(' identifier (',' identifier)* ')'
variable_decleration = identifier

-}

data Expression = Expression Term [(BinOp,Term)] deriving (Show, Eq)
data Term = Term Factor [(BinOp,Factor)] deriving (Show, Eq)
data UFactor = UFactor (Maybe UnOp) Factor deriving (Show, Eq)
data Factor = FactorVar Variable | FactorExp Expression deriving (Show, Eq)
data Variable = VariableFunc Function | VariableVar String | VariableNum Double deriving (Show, Eq)
data Function = Function [Expression] deriving (Show, Eq)
data BinOp = PlusOp | MinusOp | TimesOp | DivideOp | PowerOp deriving (Show, Eq)
data UnOp = UnMinusOp deriving (Show, Eq)

parseExpression :: [Token] -> Maybe (Expression, [Token])
parseExpression _ = Nothing

parseTerm :: [Token] -> Maybe (Term, [Token])
parseTerm _ = Nothing

parseUFactor :: [Token] -> Maybe (UFactor, [Token])
parseUFactor (Minus:xs) =
  case parseFactor xs of
    Just (fac,rst) -> Just (UFactor (Just UnMinusOp) fac, rst)
    Nothing -> Nothing
parseUFactor xs =
  case parseFactor xs of
    Just (fac,rst) -> Just (UFactor Nothing fac, rst)
    Nothing -> Nothing

parseFactor :: [Token] -> Maybe (Factor, [Token])
parseFactor xs =
  case parseVariable xs of
    Just (var,rst) -> Just (FactorVar var, rst)
    Nothing -> Nothing
-- todo: expression 

parseVariable :: [Token] -> Maybe (Variable, [Token])
parseVariable (Identifier x:xs) =
  case parseFunction (Identifier x:xs) of
    Just (fnc,rst) -> Just (VariableFunc fnc, rst)
    Nothing -> Just (VariableVar x, xs)

parseVariable (Number x:xs) = Just (VariableNum x, xs)
parseVariable _ = Nothing

parseFunction :: [Token] -> Maybe (Function, [Token])
parseFunction (Identifier x:Open:Close:xs) = Just (Function [], xs)
-- todo: with args
parseFunction _ = Nothing
