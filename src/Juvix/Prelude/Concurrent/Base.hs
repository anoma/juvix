module Juvix.Prelude.Concurrent.Base
  ( module Juvix.Prelude.Concurrent.Base,
    module Control.Concurrent.MVar,
    module Control.Monad.Catch,
    module Data.IORef,
  )
where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Monad.Catch
import Data.IORef (IORef)
import Data.IORef qualified as IO
import Juvix.Prelude.Base

newMVar :: (MonadIO m) => a -> m (MVar a)
newMVar = liftIO . MVar.newMVar

putMVar :: (MonadIO m) => MVar a -> a -> m ()
putMVar v = liftIO . MVar.putMVar v

tryPutMVar :: (MonadIO m) => MVar a -> a -> m Bool
tryPutMVar v = liftIO . MVar.tryPutMVar v

readMVar :: (MonadIO m) => MVar a -> m a
readMVar = liftIO . MVar.readMVar

takeMVar :: (MonadIO m) => MVar a -> m a
takeMVar = liftIO . MVar.takeMVar

readIORef :: (MonadIO m) => IORef a -> m a
readIORef = liftIO . IO.readIORef

newIORef :: (MonadIO m) => a -> m (IORef a)
newIORef = liftIO . IO.newIORef
