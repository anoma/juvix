module Juvix.Data.Effect.Lock.Base where

import Juvix.Prelude

data Lock m a where
  WithLock :: m a -> Lock m a

makeSem ''Lock

type ScopedLock = Scoped_ Lock
