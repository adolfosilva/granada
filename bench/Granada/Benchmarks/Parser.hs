module Granada.Benchmarks.Parser (benchmarks) where

import Criterion (Benchmark, bench, whnf, whnfIO)
import Text.Parsec (parse)
import qualified Text.Parsec.Error

import Granada.Parser (action, program, parseFromFile)
import qualified Granada.Expr

action01 :: String
action01 = "action Attack { pre: { loadedWeapon: true }, post: { enemyDamage: 10 }"

actionParse01 :: String -> Either Text.Parsec.Error.ParseError Granada.Expr.Action
actionParse01 = parse action "actionParse01" 

-- Our benchmark harness.
benchmarks :: [Benchmark]
benchmarks = [ bench "actionParse01"  $ whnf actionParse01 action01,
               bench "parseFromFile program" $ whnfIO (parseFromFile program "examples/program.grd") ]
