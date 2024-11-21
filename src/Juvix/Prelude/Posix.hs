module Juvix.Prelude.Posix where

import Control.Exception
import Juvix.Prelude.Base
import System.Posix.Signals

terminateProcessPid :: (MonadIO m, Integral a) => a -> m ()
terminateProcessPid = liftIO . signalProcess softwareTermination . fromIntegral

isProcessRunning :: forall m a. (MonadIO m, Integral a) => a -> m Bool
isProcessRunning pid = do
  result <- liftIO (try (signalProcess nullSignal (fromIntegral pid))) :: m (Either SomeException ())
  return (isRight result)
