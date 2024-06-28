{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Data.Effect.Forcing where

import Juvix.Prelude.Base

data Forcing (a :: GHCType) :: Effect where
  Forces :: NFData b => Lens' a b -> Forcing a m ()
  ForcesField :: Lens' a b -> Sem '[Forcing b] () -> Forcing a m ()

makeSem ''Forcing

forcing :: a -> Sem '[Forcing a] () -> a
forcing a = run . evalForcing a

evalForcing :: a -> Sem (Forcing a ': r) () -> Sem r a
evalForcing a =
  reinterpret (execState a) $ \case
    Forces l -> modify (over l force)
    ForcesField l r -> modify (over l (`forcing` r))
