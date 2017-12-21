module Calculator where

import Lexic
import Parser
import ErrorHandler

calculate :: String -> Double
calculate input = head $ postFixCalc (shunt (parse input) [] []) []

postFixCalc :: [Token] -> [Double] -> [Double]
postFixCalc [] xs = xs
postFixCalc (Num x:xs) ys = postFixCalc xs (x:ys)
postFixCalc (Op x:xs) (y1:y2:ys) = postFixCalc xs ((getOp x) y2 y1 : ys)

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

getOp :: (Floating a) => Char -> (a -> a -> a)
getOp '+' = (+)
getOp '-' = (-)
getOp '*' = (*)
getOp '/' = (/)
getOp '^' = (**)
