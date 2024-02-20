{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Prelude.Effects.Output where

import Data.Kind qualified as GHC
import Effectful.Dispatch.Dynamic
import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Accum
import Juvix.Prelude.Effects.Base

data Output (o :: GHC.Type) :: Effect where
  Output :: o -> Output o m ()

makeEffect ''Output

runOutputEff :: (o -> Sem r ()) -> Sem (Output o ': r) a -> Sem r a
runOutputEff handle =
  interpret $ \_ -> \case
    Output x -> handle x

runOutputList :: Sem (Output o ': r) a -> Sem r ([o], a)
runOutputList = reinterpret runAccumList $ \_ -> \case
  Output x -> accum x

execOutputList :: Sem (Output o ': r) a -> Sem r [o]
execOutputList = fmap fst . runOutputList

ignoreOutput :: Sem (Output o ': r) a -> Sem r a
ignoreOutput = interpret $ \_ -> \case
  Output {} -> return ()
