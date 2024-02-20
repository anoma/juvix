module Benchmark.Effect.Reader where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Base.Polysemy qualified as P
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
    go :: [Natural] -> Natural -> [Natural]
    go acc = \case
      0 -> acc
      m -> go (c : acc) (pred m)

countEff :: Natural -> Natural
countEff = sum' . E.runPureEff . E.runReader c . go []
  where
    go :: (E.Member (E.Reader Natural) r) => [Natural] -> Natural -> E.Sem r [Natural]
    go acc = \case
      0 -> return acc
      n -> do
        i <- E.ask
        go (i : acc) (pred n)

countSem :: Natural -> Natural
countSem = sum' . P.run . P.runReader c . go []
  where
    go :: (P.Member (P.Reader Natural) r) => [Natural] -> Natural -> P.Sem r [Natural]
    go acc = \case
      0 -> return acc
      n -> do
        i <- P.ask
        go (i : acc) (pred n)
