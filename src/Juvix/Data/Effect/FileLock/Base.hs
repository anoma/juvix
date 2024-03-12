module Juvix.Data.Effect.FileLock.Base where

import Juvix.Prelude.Base
import Juvix.Prelude.Path

-- | An effect for wrapping an action in a file lock
data FileLock m a where
  WithFileLock' :: Path Abs File -> m a -> FileLock m a

makeSem ''FileLock
