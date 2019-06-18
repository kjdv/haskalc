module Tokenizer where

import Data.Char

data Token =
    Plus
  | Minus
  | Times
  | Divide
  | Power
  | Open
  | Close
  | Comma
  | Equals
  | Identifier String
  | Number Double
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []

tokenize ('+':xs) = Plus : tokenize xs
tokenize ('-':xs) = Minus : tokenize xs
tokenize ('*':xs) = Times : tokenize xs
tokenize ('/':xs) = Divide : tokenize xs
tokenize ('^':xs) = Power : tokenize xs
tokenize ('=':xs) = Equals : tokenize xs

tokenize ('(':xs) = Open : tokenize xs
tokenize (')':xs) = Close : tokenize xs
tokenize (',':xs) = Comma : tokenize xs

tokenize (x:xs)
  | isSpace(x) = tokenize xs
  | isAlpha(x) = let (alphas,rest) = break (not . isAlpha) xs in Identifier (x:alphas) : tokenize rest
  | isDigit(x) = do
      let results = reads (x:xs)
      let (num,rest) = head results
      Number num : tokenize rest
  | otherwise  = error ("unrecognized token '" ++ [x] ++ "'")
