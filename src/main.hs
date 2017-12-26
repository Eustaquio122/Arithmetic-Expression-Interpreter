module Main where

import Lexic
import Parser
import ErrorHandler
import Calculator
import Printable

import Data.Char
import Control.Monad


process :: String -> String
process input
      | not (null errors) = printableErrors errors
      | otherwise         = printableResult result
              where errors      = getErrors parsedInput
                    result      = calculate parsedInput
                    parsedInput = parse input

main :: IO ()
main =
    putStrLn "Enter expression ('q' to quit):" >>
    getLine >>= \expression ->
                  when (expression /= "q") $ putStrLn (process expression) >> main

    
