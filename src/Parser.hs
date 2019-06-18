module Parser where

import Tokenizer

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

-- Taking insparation from Functional Pearls - Monadic Parsing in Haskell (Graham Hutton, Erik Meijer)
-- http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-- also: https://github.com/pbv/tapf-parsing

newtype Parser a = Parser ([Token] -> Maybe (a, [Token]))

instance Monad Parser where
  return a = Parser (\ts -> Just (a,ts))
  p >>= f  = Parser (apply) where
    apply ts = case parse p ts of
      Nothing -> Nothing
      Just (a,ts) -> parse (f a) ts

instance Functor Parser where
  fmap f p = do
    x <- p
    return (f x)

instance Applicative Parser where
  pure = return
  p <*> q = do
    f <- p
    x <- q
    return (f x)

parse :: Parser a -> [Token] -> Maybe (a, [Token])
parse (Parser p) = p

-- turns a token into a parser for that token
item :: Parser Token
item = Parser tokenParse where
  tokenParse ts = case ts of
    []     -> Nothing
    (t:ts) -> Just (t,ts)
