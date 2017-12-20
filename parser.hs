module Parser where

import Lexic

parseInput :: String -> [Token]
parseInput x = shunt (parse x) [] []

shunt :: [Token] -> [Token] -> [Token] -> [Token]
shunt [] [] zs             = zs
shunt [] ys zs             = zs ++ ys
shunt (Num x:xs) ys zs     = shunt xs ys (zs ++ [Num x])
shunt (Op x:xs) [] zs      = shunt xs [Op x] zs
shunt (Op x:xs) (y:ys) zs
      | y > Op x           = shunt ([Op x] ++ xs) ys (zs ++ [y])
      | otherwise          = shunt xs ([Op x] ++ (y:ys)) zs
shunt (LBr:xs) ys zs       = shunt xs (LBr:ys) zs
shunt (RBr:xs) (LBr:ys) zs = shunt xs ys zs
shunt (RBr:xs) (y:ys) zs   = shunt (RBr:xs) ys (zs ++ [y])
shunt x y z                = [Err "Parsing Error"]

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
       | otherwise    = Err [x]

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




