module Main where

import qualified Granada.Parser     as P
import           System.Environment

main :: IO ()
main = do
   args <- getArgs
   p <- P.parseFromFile P.program (args !! 0)
   print p
