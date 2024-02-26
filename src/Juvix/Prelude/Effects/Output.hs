{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Prelude.Effects.Output where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Accum
import Juvix.Prelude.Effects.Base

data Output (o :: GHCType) :: Effect where
  Output :: o -> Output o m ()

makeEffect ''Output

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
