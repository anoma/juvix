module Benchmark.State where

import Juvix.Prelude
import Juvix.Prelude.Effects (Eff, (:>))
import Juvix.Prelude.Effects qualified as E
import Test.Tasty.Bench

data St = St
  { _stA :: Natural,
    _stB :: Natural
  }

makeLenses ''St

bm :: Benchmark
bm =
  bgroup
    "State"
    [ bench "Eff State (Static)" $ nf countEff k,
      bench "Sem State" $ nf countSem k,
      bench "Raw State" $ nf countRaw k
    ]

k :: Natural
k = 2 ^ (21 :: Natural)

addSt :: St -> Natural
addSt (St a b) = a + b

emptySt :: St
emptySt = St 0 0

l :: Bool -> Lens' St Natural
l b
  | b = stA
  | otherwise = stB

countRaw :: Natural -> Natural
countRaw = addSt . go emptySt
  where
    go :: St -> Natural -> St
    go acc = \case
      0 -> acc
      m -> go (over (l (even m)) (+ m) acc) (pred m)

countEff :: Natural -> Natural
countEff = addSt . E.runPureEff . E.execState emptySt . go
  where
    go :: (E.State St :> r) => Natural -> Eff r ()
    go = \case
      0 -> return ()
      m -> E.modify (over (l (even m)) (+ m)) >> go (pred m)

countSem :: Natural -> Natural
countSem = addSt . run . execState emptySt . go
  where
    go :: (Members '[State St] r) => Natural -> Sem r ()
    go = \case
      0 -> return ()
      m -> modify (over (l (even m)) (+ m)) >> go (pred m)
