module Main where

import Printable

import Data.Char
import Control.Monad


main :: IO ()
main =
    putStrLn "\nEnter expression ('q' to quit):" >>
    getLine >>= \expression ->
                  when (expression /= "q") $ putStrLn ("Expression: " ++ expression ++ "\n" ++ "Result: " ++ process expression) >> main

    
