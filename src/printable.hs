module Printable where

import Lexic
import Data.List


printableErrors xs = intercalate "\n" (map show xs)

printableResult xs = processDot $ show xs

processDot :: String -> String
processDot []          = []
processDot ('.':xs)
         | allZeros xs = []
         | otherwise   = '.':xs
                 where allZeros = all (== '0') 
processDot (x:xs)      = x : processDot xs
