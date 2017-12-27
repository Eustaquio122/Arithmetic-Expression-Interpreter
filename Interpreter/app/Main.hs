module Main where

import Printable

import Control.Monad


main :: IO ()
main =
    putStrLn "\nEnter expression ('q' to quit):" >>
    getLine >>= \expression ->
                  when (expression /= "q") $ putStrLn ("Expression: " ++ expression ++ "\n" ++ process expression) >> main

    
