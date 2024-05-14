module Benchmark.Nockma.Encoding where

import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Stdlib (stdlib)
import Juvix.Prelude.Base
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "Jam"
    [bench "jam stdlib" $ nf runJam stdlib]

runJam :: Term Natural -> Natural
runJam =
  (^. atom)
    . fromRight (error "jam failed")
    . run
    . runError @NockNaturalNaturalError
    . jam
