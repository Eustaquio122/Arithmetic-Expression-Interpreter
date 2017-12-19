module Parser where

data Token = Op Char | Num Double | LBr | RBr | Invalid deriving (Show, Read, Eq)

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

-- postFixCalc :: 

parseInput :: String -> [Token]
parseInput x = shunt (lex' x) [] []

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

lex' :: String -> [Token]
lex' ""       = []
lex' (' ':xs) = lex' xs
lex' xs       = [headToken] ++ lex' rest
                 where headToken = getToken xs
                       rest = drop (tokenLength headToken) xs

getToken :: String -> Token
getToken (x:xs)
         | isOperator x = Op x
         | x == '('     = LBr
         | x == ')'     = RBr
         | isDigit x    = Num $ readNumber (x:xs)
         | otherwise    = Invalid

readNumber :: String -> Double
readNumber xs = read (getNumber "" xs) :: Double

getNumber :: String -> String -> String
getNumber xs (y:"")
           | isDigit y = xs ++ [y]
           | otherwise = xs
getNumber xs (y:ys)
           | isDigit y = getNumber (xs ++ [y]) ys
           | otherwise = xs

tokenLength :: Token -> Int
tokenLength (Num x)   = length $ show x
tokenLength _         = 1

isOperator x = elem x operators
isDigit x    = elem x digits




