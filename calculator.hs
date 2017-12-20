module Calculator where

import Lexic
import Parser

calculate :: String -> Double
calculate input = head $ postFixCalc (parseInput input) []

postFixCalc :: [Token] -> [Double] -> [Double]
postFixCalc [] xs = xs
postFixCalc (Num x:xs) ys = postFixCalc xs (x:ys)
postFixCalc (Op x:xs) (y1:y2:ys) = postFixCalc xs (flip (getOp x) y1 y2 : ys)

getOp :: (Floating a) => Char -> (a -> a -> a)
getOp '+' = (+)
getOp '-' = (-)
getOp '*' = (*)
getOp '/' = (/)
getOp '^' = (**)
