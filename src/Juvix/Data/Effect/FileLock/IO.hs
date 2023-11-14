module Juvix.Data.Effect.FileLock.IO where

import Juvix.Data.Effect.FileLock.Base
import Juvix.Prelude
import System.FileLock hiding (FileLock)

runFileLockIO :: (Members '[Files, Resource, Embed IO] r) => Sem (FileLock ': r) a -> Sem r a
runFileLockIO sem = do
  rootLockPath <- (<//> $(mkRelDir "juvix-file-locks")) <$> getTempDir
  ( interpretH $ \case
      WithFileLockDir p ma -> bracket (getLockFilePath rootLockPath p >>= (\f -> embed (lockFile f Exclusive))) (embed . unlockFile) (const (runTSimple ma))
    )
    sem
  where
    getLockFilePath :: (Member Files r) => Path Abs Dir -> Path Abs Dir -> Sem r FilePath
    getLockFilePath rootLockPath p = do
      ensureDir' rootLockPath
      np <- normalizeDir p
      return (toFilePath (rootLockPath <//> fromJust (parseRelFile (show (hash np)))))
