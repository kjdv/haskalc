module Parser where

import Tokenizer
import Control.Monad
import Data.Maybe

-- Parser building blocks

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

empty :: Parser a
empty = Parser (\_ -> Nothing)

end :: Parser ()
end = Parser endParse where
  endParse [] = Just ((), [])
  endParse _ = Nothing

-- turns a token into a parser for that token
item :: Parser Token
item = Parser tokenParse where
  tokenParse ts = case ts of
    []     -> Nothing
    (t:ts) -> Just (t,ts)

-- parser that applies a predicate
match :: Parser a -> (a -> Bool) -> Parser a
match base predicate = do
  t <- base
  if predicate t then return t else empty

-- parser that matches a specific token
symbol :: Token -> Parser Token
symbol tok = match item (== tok)

choice :: Parser a -> Parser a -> Parser a
choice p q = Parser choiceParse where
  choiceParse ts = (listToMaybe . catMaybes) [parse p ts, parse q ts]

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = choice (oneOrMore p) (return [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do
  a <- p
  as <- zeroOrMore p
  return (a:as)

zeroOrMoreSep :: Parser a -> Parser b -> Parser [a]
zeroOrMoreSep p s = choice (oneOrMoreSep p s) (return [])

oneOrMoreSep :: Parser a -> Parser b -> Parser [a]
oneOrMoreSep p s = do
  a <- p
  as <- zeroOrMore (s >> p)
  return (a:as)

transform :: Parser a -> (a -> b) -> Parser b
transform p f = fmap f p

maybeTransform :: Parser a -> (a -> Maybe b) -> Parser b
maybeTransform p f = Parser mt where
  mt ts = case parse (transform p f) ts of
    Nothing -> Nothing
    Just (Nothing,_) -> Nothing
    Just (Just x,rest) -> Just (x,rest)

{- Grammar:

program = (assignment | expression) EOF
assignment = decleration '=' expression
expression = term ('+' term | '-' term)*
term = factor ('^' factor | '*' factor | '/' factor)*
factor = '-' factor | (variable | '(' expression ')')

variable = function | identifier | number
function = identifier '(' expression (',' expression)* ')'

decleration = function_decleration | variable_decleration
function_decleration = identifier '(' identifier (',' identifier)* ')'
variable_decleration = identifier

-}

data Expression = EString String deriving (Show, Eq) -- todo
data Term = TString String deriving (Show, Eq) -- todo
data Factor = UFactor Unop Factor | VarFactor Variable | ExpFactor Expression deriving (Show, Eq) -- todo
data Variable = FunctionVar Function | IdentifierVar String | NumberVar Double deriving (Show, Eq)
data Function = Function String [Expression] deriving (Show, Eq)

data Unop = UMinusOp deriving (Show, Eq)
data Binop = PowerOp | TimesOp | DivideOp | PlusOp | MinusOp deriving (Show, Eq)

parseNumber :: Parser Double
parseNumber = maybeTransform item extractNumber

parseIdentifier :: Parser String
parseIdentifier = maybeTransform item extractIdentifier

parseExpression :: Parser Expression
parseExpression = transform parseIdentifier EString

parseTerm :: Parser Term
parseTerm = transform parseIdentifier TString

parseFactor :: Parser Factor
parseFactor = uparse `choice` vparse `choice` eparse where
  uparse = do
    symbol Minus
    f <- parseFactor
    return (UFactor UMinusOp f)
  vparse = transform parseVariable VarFactor
  eparse = do
    symbol Open
    e <- parseExpression
    symbol Close
    return (ExpFactor e)

parseVariable :: Parser Variable
parseVariable =
  transform parseFunction FunctionVar `choice`
  transform parseIdentifier IdentifierVar `choice`
  transform parseNumber NumberVar

parseFunction :: Parser Function
parseFunction = do
  name <- parseIdentifier
  symbol Open
  args <- zeroOrMoreSep parseExpression (symbol Comma)
  symbol Close
  return (Function name args)
