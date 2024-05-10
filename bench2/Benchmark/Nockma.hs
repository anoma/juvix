module Benchmark.Nockma where

import Benchmark.Nockma.Encoding qualified as NockmaEncoding
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "Nockma"
    [ NockmaEncoding.bm
    ]
