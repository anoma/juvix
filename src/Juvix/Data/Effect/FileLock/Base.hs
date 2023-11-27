module Juvix.Data.Effect.FileLock.Base where

import Juvix.Prelude

-- | An effect for wrapping an action in  file lock
data FileLock m a where
  WithFileLock' :: Path Abs File -> m a -> FileLock m a

makeSem ''FileLock
