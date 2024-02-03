module Juvix.Prelude.Concurrent.Lock where

import Juvix.Prelude.Base hiding (State)
import Juvix.Prelude.Concurrent.Base

newtype Lock = Lock
  { unLock :: MVar ()
  }

new :: (MonadIO m) => m Lock
new = Lock <$> newMVar ()

wait :: (MonadIO m) => Lock -> m ()
wait (Lock v) = readMVar v

acquire :: (MonadIO m) => Lock -> m ()
acquire (Lock v) = takeMVar v

release :: (MonadIO m) => Lock -> m ()
release (Lock mv) = do
  b <- tryPutMVar mv ()
  unless b (error (moduleName <> ": Can't release unlocked Lock!"))

moduleName :: Text
moduleName = "Juvix.Prelude.Concurrent.Lock"
