module Main where

import Benchmark.Effect qualified as Effect
import Juvix.Prelude
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ Effect.bm
    ]
