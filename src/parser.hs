module Parser (parse) where

import Lexic


parse :: String -> [Token]
parse xs = analise . checkInitialMinus $ tokenize xs

analise :: [Token] -> [Token]
analise []                    = []
analise (Op '+':Op y:xs)
      | y == '-'              = analise (Op '-':xs)
      | y == '+'              = analise (Op '+':xs)
      | otherwise             = (Op '+':analise (Op y:xs))
analise (Op '-':Op y:xs)
      | y == '+'              = analise (Op '-':xs)
      | y == '-'              = analise (Op '+':xs)
      | otherwise             = (Op '-':analise (Op y:xs))
analise (LBr:Op '-':Num y:xs) = (LBr:Num (-y):analise xs)
analise (x:xs)                = (x:analise xs)

checkInitialMinus :: [Token] -> [Token]
checkInitialMinus (Op '-':Num x:xs) = (Num (-x):xs)
checkInitialMinus xs                = xs

tokenize :: String -> [Token]
tokenize ""       = []
tokenize (' ':xs) = tokenize xs
tokenize xs       = (headToken:tokenize rest)
                      where headToken = elementToToken element
                            element   = getElement xs
                            rest      = drop (length element) xs

elementToToken :: String -> Token
elementToToken (x:xs)
       | x == '('      = LBr
       | x == ')'      = RBr
       | isOperator x  = Op x
       | isDigit x     = Num (read (x:xs) :: Double)
       | x == 'e'      = Err ("Error while parsing number: " ++ xs)
       | otherwise     = Err ("Invalid token: " ++ show x)

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




