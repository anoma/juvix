module Juvix.Data.Effect.Lock.NoLock where

import Juvix.Data.Effect.Lock.Base
import Juvix.Prelude
import Polysemy.Opaque

runNoLock ::
  forall r a.
  Sem (ScopedLock ': r) a ->
  Sem r a
runNoLock = interpretScopedH allocator handler
  where
    allocator :: forall q x. () -> (() -> Sem (Opaque q ': r) x) -> Sem (Opaque q ': r) x
    allocator _ use' = use' ()

    handler :: forall q r0 x. () -> Lock (Sem r0) x -> Tactical Lock (Sem r0) (Opaque q ': r) x
    handler _ = \case
      WithLock ma -> runTSimple ma
