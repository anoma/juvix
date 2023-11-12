module Juvix.Data.Effect.Lock.Resource where

import Control.Concurrent
import Juvix.Data.Effect.Lock.Base
import Juvix.Prelude
import Polysemy.Opaque

runLockResource ::
  forall r a.
  (Members '[Resource, Embed IO] r) =>
  Sem (ScopedLock ': r) a ->
  Sem r a
runLockResource = interpretScopedH allocator handler
  where
    allocator :: forall q x. () -> (MVar () -> Sem (Opaque q ': r) x) -> Sem (Opaque q ': r) x
    allocator _ use' = use' =<< embed (newMVar ())

    handler :: forall q r0 x. MVar () -> Lock (Sem r0) x -> Tactical Lock (Sem r0) (Opaque q ': r) x
    handler v = \case
      WithLock ma -> bracket_ (embed $ takeMVar v) (embed $ putMVar v ()) (runTSimple ma)
