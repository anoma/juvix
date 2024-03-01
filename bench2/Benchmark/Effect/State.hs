module Benchmark.Effect.State where

import Juvix.Prelude.Base.Foundation
import PolysemyPrelude qualified as P
import Juvix.Prelude.Effects qualified as E
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "State"
    [ bench "Eff State (Static)" $ nf countEff k,
      bench "Sem State" $ nf countSem k,
      bench "Raw State" $ nf countRaw k
    ]

k :: Natural
k = 2 ^ (22 :: Natural)

countRaw :: Natural -> Natural
countRaw = go 0
  where
    go :: Natural -> Natural -> Natural
    go acc = \case
      0 -> acc
      m -> go (acc + m) (pred m)

countEff :: Natural -> Natural
countEff = E.runPureEff . E.execState 0 . go
  where
    go :: (E.Member (E.State Natural) r) => Natural -> E.Sem r ()
    go = \case
      0 -> return ()
      m -> E.modify (+ m) >> go (pred m)

countSem :: Natural -> Natural
countSem = P.run . P.execState 0 . go
  where
    go :: (P.Members '[P.State Natural] r) => Natural -> P.Sem r ()
    go = \case
      0 -> return ()
      m -> P.modify (+ m) >> go (pred m)
