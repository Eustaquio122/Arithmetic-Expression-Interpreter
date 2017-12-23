module Calculator where --(calculate) where

import Lexic
import Parser


calculate :: [Token] -> Double
calculate parsedInput = postFixCalc (shunt parsedInput []) []

postFixCalc :: [Token] -> [Double] -> Double
postFixCalc [] xs                = head xs
postFixCalc (Num x:xs) ys        = postFixCalc xs ([x] ++ ys)
postFixCalc (Op x:xs) (y1:y2:ys) = postFixCalc xs ((getOp x) y2 y1 : ys)

shunt :: [Token] -> [Token] -> [Token]
shunt [] []             = []
shunt [] ys             = ys
shunt (Num x:xs) ys     = [Num x] ++ shunt xs ys
shunt (Op x:xs) []      = shunt xs [Op x]
shunt (Op x:xs) (y:ys)
      | Op x <= y        = [y] ++ shunt (Op x:xs) ys
      | otherwise       = shunt xs (Op x:y:ys)
shunt (LBr:xs) ys       = shunt xs (LBr:ys)
shunt (RBr:xs) (LBr:ys) = shunt xs ys
shunt (RBr:xs) (y:ys)   = [y] ++ shunt (RBr:xs) ys
shunt x y               = [Err ("Parsing Error: " ++ show x ++ " " ++ show y)]

getOp :: (Floating a) => Char -> (a -> a -> a)
getOp '+' = (+)
getOp '-' = (-)
getOp '*' = (*)
getOp '/' = (/)
getOp '^' = (**)
