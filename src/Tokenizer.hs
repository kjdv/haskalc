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

tappend :: Token -> Maybe [Token] -> Maybe [Token]
tappend t mts = fmap (\ts -> t : ts) mts

tokenize :: String -> Maybe [Token]
tokenize [] = Just []

tokenize ('+':xs) = tappend Plus $ tokenize xs
tokenize ('-':xs) = tappend Minus $ tokenize xs
tokenize ('*':xs) = tappend Times $ tokenize xs
tokenize ('/':xs) = tappend Divide $ tokenize xs
tokenize ('^':xs) = tappend Power $ tokenize xs
tokenize ('=':xs) = tappend Equals $ tokenize xs

tokenize ('(':xs) = tappend Open $ tokenize xs
tokenize (')':xs) = tappend Close $ tokenize xs
tokenize (',':xs) = tappend Comma $ tokenize xs

tokenize (x:xs)
  | isSpace(x) = tokenize xs
  | isAlpha(x) = let (alphas,rest) = break (\c -> (not . isAlphaNum) c && (c /= '_')) xs
                  in tappend (Identifier (x:alphas)) $ tokenize rest
  | isDigit(x) = do
      let results = reads (x:xs)
      let (num,rest) = head results
      tappend (Number num) $ tokenize rest
  | otherwise = Nothing

extractNumber :: Token -> Maybe Double
extractNumber (Number n) = Just n
extractNumber _ = Nothing

extractIdentifier :: Token -> Maybe String
extractIdentifier (Identifier s) = Just s
extractIdentifier _ = Nothing
