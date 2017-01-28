module Main where

import System.Environment
import Lib (readExpr)
import Parser (goal, action, parseFromFile)

main :: IO ()
main = do
   args <- getArgs
   action <- parseFromFile goal (args !! 0)
   print action
