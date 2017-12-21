{-# LANGUAGE DeriveDataTypeable #-}

module ErrorHandler where

import Lexic
import Data.Data

validate :: [Token] -> Bool
validate [] = True
validate _  = False

getErrors :: [Token] -> [Token]
getErrors xs = checkBrackets xs ++ checkFirst (head xs) ++ checkSequence xs ++ checkLast (last xs)

checkBrackets :: [Token] -> [Token]
checkBrackets xs = bracketErrList $ countBrackets xs

bracketErrList :: Int -> [Token]
bracketErrList x
             | x == 0 = []
             | x > 0  = [Err (show x ++ " unmatched '('\n")]
             | x < 0  = [Err (show (abs x) ++ " unmatched ')'\n")]

countBrackets :: [Token] -> Int
countBrackets [] = 0
countBrackets (x:xs)
            | show x == "(" = 1 + countBrackets xs
            | show x == ")" = countBrackets xs - 1
            | otherwise     = countBrackets xs

checkFirst :: Token -> [Token]
checkFirst x
         | isNum x   = []
         | isLBr x   = []
         | isErr x   = [Err ("Invalid token: " ++ show x ++ "\n")]
         | otherwise = [Err ("Parsing error: expression cannot start with: " ++ show x ++ "\n")]

checkSequence :: [Token] -> [Token]
checkSequence []  = []
checkSequence [x] = []
checkSequence (x1:x2:xs)
            | isNum x1 && (isOp x2 || isBr x2)    = [] ++ checkSequence xs
            | isLBr x1 && (isNum x2 || isLBr x2)  = [] ++ checkSequence xs
            | isRBr x1 && (isOp x2 || isRBr x2)   = [] ++ checkSequence xs
            | isOp x1 && (isNum x2 || isLBr x2)   = [] ++ checkSequence xs
            | otherwise                           = [Err (sequenceError x1 x2)] ++ checkSequence xs
                    where sequenceError x y =  "Invalid token sequence: " ++ show x1 ++ " " ++ show x2 ++ "\n"

checkLast :: Token -> [Token]
checkLast x
        | isNum x   = []
        | isRBr x   = []
        | isErr x   = [Err ("Invalid token: " ++ show x ++ "\n")]
        | otherwise = [Err ("Parsing error: expression cannot end with: " ++ show x ++ "\n")]

