-- | Inspired by Control.Concurrent.ReadWriteVar
module Juvix.Prelude.Concurrent.ReadWriteVar where

import Juvix.Prelude.Base
import Juvix.Prelude.Concurrent.Base
import Juvix.Prelude.Concurrent.ReadWriteLock (RWLock)
import Juvix.Prelude.Concurrent.ReadWriteLock qualified as RWL

data RWVar a = RWVar
  { _rwVarLock :: RWLock,
    _rwVarLockRef :: IORef a
  }

new :: (MonadIO m) => a -> m (RWVar a)
new a = do
  r <- liftIO (newIORef a)
  l <- liftIO RWL.new
  return
    RWVar
      { _rwVarLock = l,
        _rwVarLockRef = r
      }

with :: (MonadIO m, MonadMask m) => RWVar a -> (a -> m b) -> m b
with RWVar {..} f = RWL.withRead _rwVarLock (readIORef _rwVarLockRef >>= f)
