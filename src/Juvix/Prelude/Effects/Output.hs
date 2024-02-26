{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Prelude.Effects.Output where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Accum
import Juvix.Prelude.Effects.Base

data Output (o :: GHCType) :: Effect where
  Output :: o -> Output o m ()

makeEffect ''Output

runOutputFold :: o -> (o -> o -> o) -> Sem (Output o ': r) a -> Sem r (o, a)
runOutputFold ini f =
  reinterpret (runState ini) $ \case
    Output x -> modify (\acc -> f acc x)

runOutputMonoidL :: (Monoid o) => Sem (Output o ': r) a -> Sem r (o, a)
runOutputMonoidL = runOutputFold mempty (\acc x -> x <> acc)

runOutputMonoidR :: (Monoid o) => Sem (Output o ': r) a -> Sem r (o, a)
runOutputMonoidR = runOutputFold mempty (\acc x -> acc <> x)

runOutputSem :: (o -> Sem r ()) -> Sem (Output o ': r) a -> Sem r a
runOutputSem handle =
  interpret $ \case
    Output x -> handle x

runOutputList :: Sem (Output o ': r) a -> Sem r ([o], a)
runOutputList = reinterpret runAccumList $ \case
  Output x -> accum x

execOutputList :: Sem (Output o ': r) a -> Sem r [o]
execOutputList = fmap fst . runOutputList

ignoreOutput :: Sem (Output o ': r) a -> Sem r a
ignoreOutput = interpret $ \case
  Output {} -> return ()
