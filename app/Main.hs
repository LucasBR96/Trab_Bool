module Main where

import Lib(generate_tab)
main :: IO()
main = do
    l <- getLine
    if null l 
        then return ()
        else do
            putStr (  "\n" ++ generate_tab l ++ "\n" )
            main