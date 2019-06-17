module Tokenize where

data Token =
    Plus
  | Minus
  | Times
  | Divide
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('+':xs) = Plus : tokenize xs
tokenize (x:_) = error ("unrecognized token '" ++ [x] ++ "'")
