module Benchmark.Nockma.Encoding where

import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Stdlib (stdlib)
import Juvix.Prelude.Base
import Test.Tasty.Bench

jamStdlib :: Natural
jamStdlib = runJam stdlib

bm :: Benchmark
bm =
  bgroup
    "Jam"
    [ bench "jam stdlib" $ nf runJam stdlib,
      bench "cue (jam stdlib)" $ nf runCue jamStdlib
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
