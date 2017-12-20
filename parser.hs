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
shunt _ _ _                = [Invalid]

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
       | otherwise    = Invalid

getElement :: String -> String
getElement (x:xs)
         | isDigit x = getNumber "" (x:xs)
         | otherwise = [x]

-- readNumber :: String -> Double
-- readNumber xs = read (getNumber "" xs) :: Double

getNumber :: String -> String -> String
getNumber xs ('.':y:ys)
        | isDigit y = getNumber (xs ++ ['.', y]) ys
        | otherwise = "§"
getNumber (x:xs) "" = [x]
getNumber xs (y:ys)
        | isDigit y = getNumber (xs ++ [y]) ys
        | otherwise = xs

-- elementLength :: Token -> Int
-- elementLength (Num x)   = (length $ show x)
-- elementLength _         = 1

isOperator x = elem x operators
isDigit x    = elem x digits




