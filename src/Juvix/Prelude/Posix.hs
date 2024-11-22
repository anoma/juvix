module Juvix.Prelude.Posix where

import Juvix.Prelude.Base
import System.Posix.Signals

terminateProcessPid :: (MonadIO m, Integral a) => a -> m ()
terminateProcessPid = liftIO . signalProcess softwareTermination . fromIntegral
