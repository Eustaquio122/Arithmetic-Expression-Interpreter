module Printable where

import Lexic
import Parser
import ErrorHandler
import Calculator

import Data.List


process :: String -> String
process input
      | not (null errors) = printableErrors errors
      | otherwise         = "Result: " ++ printableResult result
              where errors      = getErrors parsedInput
                    result      = calculate parsedInput
                    parsedInput = parse input

printableErrors :: [Token] -> String
printableErrors xs = intercalate "\n" (map show xs)

printableResult :: Double -> String
printableResult xs = processDot $ show xs

