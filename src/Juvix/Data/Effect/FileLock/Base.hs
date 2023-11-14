module Juvix.Data.Effect.FileLock.Base where

import Juvix.Prelude

data FileLock m a where
  WithFileLockDir :: Path Abs Dir -> m a -> FileLock m a

makeSem ''FileLock
