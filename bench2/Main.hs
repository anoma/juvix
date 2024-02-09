module Main where

import Benchmark.Output qualified as Output
import Benchmark.State qualified as State
import Juvix.Prelude
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ Output.bm,
      State.bm
    ]
