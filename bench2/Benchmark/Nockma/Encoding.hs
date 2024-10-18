module Benchmark.Nockma.Encoding where

import Juvix.Compiler.Nockma.AnomaLib (anomaLib)
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import Test.Tasty.Bench

jamStdlib :: Natural
jamStdlib = runJam anomaLib

bm :: Benchmark
bm =
  bgroup
    "Jam"
    [ bench "jam anomaLib" $ nf runJam anomaLib,
      bench "cue (jam anomaLib)" $ nf runCue jamStdlib
    ]

runJam :: Term Natural -> Natural
runJam =
  (^. atom)
    . fromRight (error "jam failed")
    . run
    . runError @NockNaturalNaturalError
    . jam

mkAtom :: Natural -> Atom Natural
mkAtom n =
  Atom
    { _atomInfo = emptyAtomInfo,
      _atom = n
    }

runCue :: Natural -> Term Natural
runCue =
  run
    . runErrorNoCallStackWith @NockNaturalNaturalError (const (error "NockNaturalNaturalError"))
    . runErrorNoCallStackWith @DecodingError (const (error "decoding failed"))
    . cue
    . mkAtom
