module Benchmark.Output where

import Juvix.Prelude
import Juvix.Prelude.Effects (Eff, (:>))
import Juvix.Prelude.Effects qualified as E
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "Output"
    [ bench "Eff Output (Dynamic)" $ nf countdownEff k,
      bench "Eff Accum (Static)" $ nf countdownAccum k,
      bench "Sem Output" $ nf countdownSem k,
      bench "Raw Output" $ nf countdownRaw k
    ]

k :: Natural
k = 2 ^ (22 :: Natural)

countdownRaw :: Natural -> Natural
countdownRaw = sum' . reverse . go []
  where
    go :: [Natural] -> Natural -> [Natural]
    go acc = \case
      0 -> acc
      m -> go (m : acc) (pred m)

countdownAccum :: Natural -> Natural
countdownAccum = sum' . E.runPureEff . E.execAccumList . go
  where
    go :: (E.Accum Natural :> r) => Natural -> Eff r ()
    go = \case
      0 -> return ()
      m -> E.accum m >> go (pred m)

countdownEff :: Natural -> Natural
countdownEff = sum' . E.runPureEff . E.execOutputList . go
  where
    go :: (E.Output Natural :> r) => Natural -> Eff r ()
    go = \case
      0 -> return ()
      m -> E.output m >> go (pred m)

countdownSem :: Natural -> Natural
countdownSem = sum' . run . execOutputList . go
  where
    go :: (Members '[Output Natural] r) => Natural -> Sem r ()
    go = \case
      0 -> return ()
      m -> output m >> go (pred m)
