{-# LANGUAGE DeriveDataTypeable #-}

module Lexic where

import Data.Data

data Token = Op Char | Num Double | LBr | RBr | Err [Char] deriving (Read, Eq, Typeable, Data)

instance Ord Token where
  compare (Op x) (Op y)   = compare (prec x) (prec y)
  compare (Num x) (Num y) = compare x y
  compare _ _             = LT

instance Show Token where
  show (Op x)  = show x
  show (Num x) = show x
  show LBr     = "("
  show RBr     = ")"
  show (Err x) = show x

prec :: Char -> Int
prec '+' = 0
prec '-' = 0
prec '*' = 1
prec '/' = 1
prec '^' = 2

operators = "+-*/^"
digits    = "0123456789"

opConstr  = toConstr (Op undefined)
numConstr = toConstr (Num undefined)
lbrConstr = toConstr LBr
rbrConstr = toConstr RBr
errConstr = toConstr (Err undefined)

isOp :: Token -> Bool
isOp x
   | toConstr x == opConstr = True
   | otherwise              = False

isNum :: Token -> Bool
isNum x
   | toConstr x == numConstr = True
   | otherwise               = False

isLBr :: Token -> Bool
isLBr x
   | toConstr x == lbrConstr = True
   | otherwise               = False

isRBr :: Token -> Bool
isRBr x
   | toConstr x == rbrConstr = True
   | otherwise               = False

isErr :: Token -> Bool
isErr x
   | toConstr x == errConstr = True
   | otherwise               = False

isBr x = isLBr x || isRBr x


