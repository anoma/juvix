{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Prelude.Effects.Input where

import Data.Stream qualified as Stream
import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base
import Juvix.Prelude.Stream

-- TODO make static versions. Finite and infinite.
data Input (i :: GHCType) :: Effect where
  Input :: Input i m i

makeEffect ''Input

runInputList :: forall i r a. [i] -> Sem (Input (Maybe i) ': r) a -> Sem r a
runInputList s = reinterpret (evalState s) $ \case
  Input -> do
    x <- gets @[i] nonEmpty
    case x of
      Nothing -> return Nothing
      Just (a :| as) -> do
        put as
        return (Just a)

runInputStream :: forall i r a. Stream i -> Sem (Input i ': r) a -> Sem r a
runInputStream s = reinterpret (evalState s) $ \case
  Input -> do
    Stream.Cons a as <- get @(Stream i)
    put as
    return a

runInputNaturals :: Sem (Input Natural ': r) a -> Sem r a
runInputNaturals = runInputStream allNaturals
