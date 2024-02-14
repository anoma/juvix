module Benchmark.Effect.ReaderH where

import Juvix.Prelude
import Juvix.Prelude.Effects (Eff, (:>))
import Juvix.Prelude.Effects qualified as E
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "Reader (Higher order)"
    [ bench "Eff Reader (Static)" $ nf countEff k,
      bench "Sem Reader" $ nf countSem k,
      bench "Raw Reader" $ nf countRaw k
    ]

k :: Natural
k = 2 ^ (21 :: Natural)

countRaw :: Natural -> Natural
countRaw = sum' . go []
  where
    go :: [Natural] -> Natural -> [Natural]
    go acc = \case
      0 -> acc
      m -> go (m : acc) (pred m)

countEff :: Natural -> Natural
countEff x = sum' . E.runPureEff . E.runReader x $ go []
  where
    go :: (E.Reader Natural :> r) => [Natural] -> Eff r [Natural]
    go acc = do
      n <- E.ask
      case n of
        0 -> return acc
        m -> E.local @Natural pred (go (m : acc))

countSem :: Natural -> Natural
countSem x = sum . run . runReader x $ go []
  where
    go :: (Members '[Reader Natural] r) => [Natural] -> Sem r [Natural]
    go acc = do
      n :: Natural <- ask
      case n of
        0 -> return acc
        m -> local @Natural pred (go (m : acc))
