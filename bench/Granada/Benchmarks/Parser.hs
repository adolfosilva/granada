module Granada.Benchmarks.Parser (benchmarks) where

import Criterion (Benchmark, bench, whnf)
import Text.Parsec (parse)
import qualified Text.Parsec.Error

import Granada.Parser (action)
import qualified Granada.Expr

action01 :: String
action01 = "action Attack { pre: { loadedWeapon: true }, post: { enemyDamage: 10 }"

actionParse01 :: String -> Either Text.Parsec.Error.ParseError Granada.Expr.Action
actionParse01 = parse action "actionParse01" 

-- Our benchmark harness.
benchmarks :: [Benchmark]
benchmarks = [ bench "1"  $ whnf actionParse01 action01 ]
