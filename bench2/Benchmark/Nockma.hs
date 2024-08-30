module Benchmark.Nockma where

import Benchmark.Nockma.Encoding qualified as NockmaEncoding
import Benchmark.Nockma.Encoding.ByteString qualified as NockmaByteString
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "Nockma"
    [ NockmaEncoding.bm,
      NockmaByteString.bm
    ]
