{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

-- | This effect provides convenient syntax for individually forcing evaluation
-- on fields of a record type (or anything pointed by a lens)
module Juvix.Data.Effect.Forcing where

import Juvix.Prelude.Base

data Forcing (a :: GHCType) :: Effect where
  -- | Forces full evaluation on the field pointed by the lens
  ForcesField :: (NFData b) => Lens' a b -> Forcing a m ()
  -- | Forcing effect scoped to the field pointed by the lens
  Forces :: Lens' a b -> Sem '[Forcing b] () -> Forcing a m ()

makeSem ''Forcing

forcing :: a -> Sem '[Forcing a] () -> a
forcing a = run . evalForcing a

evalForcing :: a -> Sem (Forcing a ': r) () -> Sem r a
evalForcing a =
  reinterpret (execState a) $ \case
    ForcesField l -> modify (over l force)
    Forces l r -> modify (over l (`forcing` r))
