module Parser where

import Lexic

parse :: String -> [Token]
parse ""       = []
parse (' ':xs) = parse xs
parse xs       = [headToken] ++ parse rest
                 where headToken = tokenize element
                       element   = getElement xs
                       rest      = drop (length element) xs

tokenize :: String -> Token
tokenize (x:xs)
       | x == '('     = LBr
       | x == ')'     = RBr
       | isOperator x = Op x
       | isDigit x    = Num (read (x:xs) :: Double)
       | x == 'e'     = Err ("Error while parsing number: " ++ xs)
       | otherwise    = Err ("Invalid token: " ++ show x ++ "\n")

getElement :: String -> String
getElement (x:xs)
         | isDigit x    = getNumber "" (x:xs)
         | otherwise    = [x]

getNumber :: String -> String -> String
getNumber xs ('.':y:ys)
        | isDigit y = getNumber (xs ++ ['.', y]) ys
        | otherwise = ('e':xs) ++ ['.', y]
getNumber (x:xs) "" = [x]
getNumber xs (y:ys)
        | isDigit y = getNumber (xs ++ [y]) ys
        | otherwise = xs

isOperator x = elem x operators
isDigit x    = elem x digits




