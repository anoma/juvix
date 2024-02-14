module Benchmark.Effect where

import Benchmark.Effect.EmbedIO qualified as EmbedIO
import Benchmark.Effect.Output qualified as Output
import Benchmark.Effect.Reader qualified as Reader
import Benchmark.Effect.ReaderH qualified as ReaderH
import Benchmark.Effect.State qualified as State
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "Effect"
    [ Output.bm,
      State.bm,
      ReaderH.bm,
      EmbedIO.bm,
      Reader.bm
    ]
