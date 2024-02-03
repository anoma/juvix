module Juvix.Prelude.Concurrent.ReadWriteLock where

import Juvix.Prelude.Base hiding (State, bracket_)
import Juvix.Prelude.Concurrent.Base
import Juvix.Prelude.Concurrent.Lock (Lock)
import Juvix.Prelude.Concurrent.Lock qualified as Lock
import Prelude (($!))

data State
  = Free
  | Read Int
  | Write

data RWLock = RWLock
  { state :: MVar State,
    readLock :: Lock,
    writeLock :: Lock
  }

new :: (MonadIO m) => m RWLock
new =
  liftA3
    RWLock
    (newMVar Free)
    Lock.new
    Lock.new

withRead :: (MonadIO m, MonadMask m) => RWLock -> m a -> m a
withRead = liftA2 bracket_ acquireRead releaseRead

acquireRead :: (MonadMask m, MonadIO m) => RWLock -> m ()
acquireRead
  (RWLock {state, readLock, writeLock}) = mask_ acqRead
    where
      acqRead = do
        st <- takeMVar state
        case st of
          Free -> do
            Lock.acquire readLock
            putMVar state $ Read 1
          Read n -> putMVar state . Read $! succ n
          Write -> do
            putMVar state st
            Lock.wait writeLock
            acqRead

releaseRead :: (MonadIO m, MonadMask m) => RWLock -> m ()
releaseRead RWLock {..} = mask_ $ do
  st <- takeMVar state
  case st of
    Read 1 -> do
      Lock.release readLock
      putMVar state Free
    Read n -> putMVar state . Read $! pred n
    _ -> do
      putMVar state st
      error (moduleName <> ".releaseRead: already released")

moduleName :: Text
moduleName = "Juvix.Prelude.Concurrent.ReadWriteLock"
