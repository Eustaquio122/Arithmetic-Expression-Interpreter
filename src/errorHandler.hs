{-# LANGUAGE DeriveDataTypeable #-}

module ErrorHandler (getErrors)  where

import Lexic
import Data.Data

validate :: [Token] -> Bool
validate [] = True
validate _  = False

getErrors :: [Token] -> [Token]
getErrors xs
        | not (null parsingErrors) = parsingErrors
        | not (null syntaxErrors)  = syntaxErrors
        | otherwise                = []
                where parsingErrors = getParsingErrors xs
                      syntaxErrors  = getSyntaxErrors xs
                  
getParsingErrors :: [Token] -> [Token]
getParsingErrors [] = []
getParsingErrors (x:xs)
               | isErr x   = [x] ++ getParsingErrors xs
               | otherwise = getParsingErrors xs

getSyntaxErrors :: [Token] -> [Token]
getSyntaxErrors xs = checkBrackets xs ++ checkFirst (head xs) ++ checkSequence xs ++ checkLast (last xs)

checkBrackets :: [Token] -> [Token]
checkBrackets xs = bracketErrList $ countBrackets xs

bracketErrList :: Int -> [Token]
bracketErrList x
             | x == 0 = []
             | x > 0  = [Err ("Syntax error: " ++ show x ++ " unmatched '('")]
             | x < 0  = [Err ("Syntax error: " ++ show (abs x) ++ " unmatched ')'")]

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
         | otherwise = [Err ("Syntax error: expression cannot start with: " ++ show x)]

checkSequence :: [Token] -> [Token]
checkSequence []  = []
checkSequence [x] = []
checkSequence (x1:x2:xs)
            | isNum x1 && (isOp x2 || isBr x2)    = [] ++ checkSequence xs
            | isLBr x1 && (isNum x2 || isLBr x2)  = [] ++ checkSequence xs
            | isRBr x1 && (isOp x2 || isRBr x2)   = [] ++ checkSequence xs
            | isOp x1 && (isNum x2 || isLBr x2)   = [] ++ checkSequence xs
            | otherwise                           = [Err (sequenceError x1 x2)] ++ checkSequence xs
                    where sequenceError x y =  "Syntax error: Invalid token sequence: " ++ show x1 ++ " " ++ show x2

checkLast :: Token -> [Token]
checkLast x
        | isNum x   = []
        | isRBr x   = []
        | otherwise = [Err ("Syntax error: expression cannot end with: " ++ show x)]

