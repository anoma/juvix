module Juvix.Data.Effect.FileLock.Base where

import Juvix.Prelude.Base
import Juvix.Prelude.Path

-- | An effect for wrapping an action in  file lock
data FileLock :: Effect where
  WithFileLock' :: Path Abs File -> m a -> FileLock m a

makeSem ''FileLock
