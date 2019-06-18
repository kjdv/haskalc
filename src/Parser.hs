module Parser where

import Tokenizer

import Control.Monad
import Control.Applicative

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

-- Taking insparation from Functional Pearls - Monadic Parsing in Haskell (Graham Hutton, Erik Meijer) - http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf

newtype Parser a = Parser ([Token] -> [(a, [Token])])

instance Functor Parser where
  fmap f (Parser ts) = Parser (\s -> [(f a, b) | (a, b) <- ts s])

instance Applicative Parser where
  pure = return
  (Parser ts1) <*> (Parser ts2) = Parser (\s -> [(f a, s2) | (f, s1) <- ts1 s, (a, s2) <- ts2 s1])

instance Monad Parser where
  return a = Parser (\ts -> [(a,ts)])
  p >>= f  = Parser (\ts -> concat [parse (f a) ts' | (a,ts') <- parse p ts]) where
    parse (Parser p) = p

-- turns a token into a parser for that token
item :: Parser Token
item = Parser tokenParse where
  tokenParse ts = case ts of
    []     -> []
    (t:ts) -> [(t,ts)]
