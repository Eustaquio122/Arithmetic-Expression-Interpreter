{-# LANGUAGE DeriveDataTypeable #-}

module ErrorHandler where

import Lexic
import Data.Data

validate :: [Token] -> Bool
validate [] = True
validate _  = False

getErrors :: [Token] -> [Token]
getErrors xs = checkBrackets xs ++ checkFirst xs ++ checkSequence xs ++ checkLast xs

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

checkFirst :: [Token] -> [Token]
checkFirst (x:xs)
         | toConstr x == numConstr = []
         | toConstr x == lbrConstr = []
         | toConstr x == errConstr = [Err ("Invalid token: " ++ show x ++ "\n")]
         | otherwise         = [Err ("Parsing error: expression cannot start with: " ++ show x ++ "\n")]

checkSequence :: [Token] -> [Token]
checkSequence [x] = []
checkSequence [x, y]
            | current == numConstr && (next == opConstr || next == rbrConstr || next == lbrConstr) = []
            | current == lbrConstr && (next == numConstr || next == lbrConstr)                     = []
            | current == rbrConstr && (next == opConstr || next == rbrConstr)                      = []
            | current == opConstr && (next == numConstr || next == lbrConstr)                      = []
            | otherwise                                                                            = [Err (sequenceError x y)]
                       where current           = toConstr x
                             next              = toConstr y
                             sequenceError x y =  "Invalid token sequence: " ++ show x ++ " " ++ show y ++ "\n"
checkSequence (x1:x2:xs)
            | current == numConstr && (next == opConstr || next == rbrConstr || next == lbrConstr) = [] ++ checkSequence xs
            | current == lbrConstr && (next == numConstr || next == lbrConstr)                     = [] ++ checkSequence xs
            | current == rbrConstr && (next == opConstr || next == rbrConstr)                      = [] ++ checkSequence xs
            | current == opConstr && (next == numConstr || next == lbrConstr)                      = [] ++ checkSequence xs
            | otherwise                                                     = [Err (sequenceError x1 x2)] ++ checkSequence xs
                       where current           = toConstr x1
                             next              = toConstr x2
                             sequenceError x y =  "Invalid token sequence: " ++ show x1 ++ " " ++ show x2 ++ "\n"

checkLast :: [Token] -> [Token]
checkLast (xs:x)
            | toConstr x == numConstr = []
            | toConstr x == lbrConstr = []
            | toConstr x == errConstr = [Err ("Invalid token: " ++ show x ++ "\n")]
            | otherwise               = [Err ("Parsing error: expression cannot end with: " ++ show x ++ "\n")]

