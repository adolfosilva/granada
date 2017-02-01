module Main where

import           Criterion.Main (bgroup, defaultMain)

import qualified Granada.Benchmarks.Parser as Parser

main :: IO ()
main = defaultMain [bgroup "Granada.Parser" Parser.benchmarks]
