module Parser (benchmarks) where

import Criterion (Benchmark, bench, whnf)
import qualified Granada

-- The function we're benchmarking.
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

-- Our benchmark harness.
benchmarks = [ bench "1"  $ whnf fib 1
             , bench "5"  $ whnf fib 5
             , bench "9"  $ whnf fib 9
             , bench "11" $ whnf fib 11 ]
