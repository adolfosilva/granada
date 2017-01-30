module Main where

import Criterion.Main (bgroup, defaultMain)
import qualified Parser

main :: IO ()
main = defaultMain [ bgroup "Parser" Parser.benchmarks]
