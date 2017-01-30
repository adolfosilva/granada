module Main where

import System.Environment
import Granada.Parser (program, goal, action, character, parseFromFile)

main :: IO ()
main = do
   args <- getArgs
   p <- parseFromFile program (args !! 0)
   print p
