module Lexer (
  Token,
  lex',
)  where

data Token = Op Char | Num Int | LBr | RBr | Invalid deriving (Show, Read, Eq)

instance Ord Token where
  compare (Op '*') (Op '-') = GT
  compare (Op '*') (Op '+') = GT
  compare (Op '/') (Op '-') = GT
  compare (Op '/') (Op '+') = GT
  compare _ _               = LT


operators = "+-*/"
digits    = "0123456789"


lex' :: String -> [Token]
lex' "" = []
lex' (' ':xs) = lex' xs
lex' xs = [headToken] ++ lex' rest
           where headToken = getToken xs
                 rest = drop (tokenLength headToken) xs

getToken :: String -> Token
getToken (x:xs)
         | isOperator x = Op x
         | x == '('     = LBr
         | x == ')'     = RBr
         | isDigit x    = Num $ readNumber (x:xs)
         | otherwise    = Invalid

readNumber :: String -> Int
readNumber xs = read (getNumber "" xs) :: Int

getNumber :: String -> String -> String
getNumber xs (y:ys)
           | isDigit y = getNumber (xs ++ [y]) ys
           | otherwise = xs

tokenLength :: Token -> Int
tokenLength (Num x)   = length $ show x
tokenLength _         = 1

isOperator x = elem x operators
isDigit x    = elem x digits

