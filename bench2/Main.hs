module Main where

import Benchmark.Effect qualified as Effect
import Benchmark.Nockma qualified as Nockma
import Benchmark.Nockma.Encoding.Natural as Natural
import Juvix.Prelude
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ Effect.bm,
      Nockma.bm,
      Natural.bm
    ]
