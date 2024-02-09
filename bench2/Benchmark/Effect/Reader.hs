module Benchmark.Effect.Reader where

import Juvix.Prelude
import Juvix.Prelude.Effects (Eff, (:>))
import Juvix.Prelude.Effects qualified as E
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "Reader (First order)"
    [ bench "Eff Reader (Static)" $ nf countEff k,
      bench "Sem Reader" $ nf countSem k,
      bench "Raw Reader" $ nf countRaw k
    ]

k :: Natural
k = 2 ^ (21 :: Natural)

c :: Natural
c = 5

countRaw :: Natural -> Natural
countRaw = sum' . go []
  where
    i :: Natural = 5
    go :: [Natural] -> Natural -> [Natural]
    go acc = \case
      0 -> acc
      m -> go (i : acc) (pred m)

countEff :: Natural -> Natural
countEff = sum' . E.runPureEff . E.runReader c . go []
  where
    go :: (E.Reader Natural :> r) => [Natural] -> Natural -> Eff r [Natural]
    go acc = \case
      0 -> return acc
      n -> do
        i <- E.ask
        go (i : acc) (pred n)

countSem :: Natural -> Natural
countSem = sum' . run . runReader c . go []
  where
    go :: (Member (Reader Natural) r) => [Natural] -> Natural -> Sem r [Natural]
    go acc = \case
      0 -> return acc
      n -> do
        i <- ask
        go (i : acc) (pred n)
