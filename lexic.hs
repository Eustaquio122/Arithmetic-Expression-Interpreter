module Lexic where

data Token = Op Char | Num Double | LBr | RBr | Err [Char] deriving (Show, Read, Eq)

instance Ord Token where
  compare (Op x) (Op y)   = compare (prec x) (prec y)
  compare (Num x) (Num y) = compare x y
  compare _ _             = LT

prec :: Char -> Int
prec '+' = 0
prec '-' = 0
prec '*' = 1
prec '/' = 1
prec '^' = 2

operators = "+-*/^"
digits    = "0123456789"

