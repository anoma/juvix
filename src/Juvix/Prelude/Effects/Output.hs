{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Prelude.Effects.Output where

import Data.Kind qualified as GHC
import Effectful.Dispatch.Dynamic
import Juvix.Prelude.Base hiding (Effect, Output, interpret, output, reinterpret, runOutputList)
import Juvix.Prelude.Effects.Accum
import Juvix.Prelude.Effects.Base

data Output (o :: GHC.Type) :: Effect where
  Output :: o -> Output o m ()

makeEffect ''Output

runOutputEff :: (o -> Eff r ()) -> Eff (Output o ': r) a -> Eff r a
runOutputEff handle =
  interpret $ \_ -> \case
    Output x -> handle x

runOutputList :: Eff (Output o ': r) a -> Eff r ([o], a)
runOutputList = reinterpret runAccumList $ \_ -> \case
  Output x -> accum x

ignoreOutput :: Eff (Output o ': r) a -> Eff r a
ignoreOutput = interpret $ \_ -> \case
  Output {} -> return ()
