{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Data.Effect.Forcing where

import Juvix.Prelude.Base

data Forcing (a :: GHCType) :: Effect where
  ForcesField :: (NFData b) => Lens' a b -> Forcing a m ()
  Forces :: Lens' a b -> Sem '[Forcing b] () -> Forcing a m ()

makeSem ''Forcing

forcing :: a -> Sem '[Forcing a] () -> a
forcing a = run . evalForcing a

evalForcing :: a -> Sem (Forcing a ': r) () -> Sem r a
evalForcing a =
  reinterpret (execState a) $ \case
    ForcesField l -> modify (over l force)
    Forces l r -> modify (over l (`forcing` r))
