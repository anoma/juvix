module Juvix.Data.Effect.FileLock.IO where

import Juvix.Data.Effect.FileLock.Base
import Juvix.Prelude
import System.FileLock hiding (FileLock)

-- | Interpret `FileLock` using `System.FileLock`
runFileLockIO :: (Members '[Resource, Embed IO] r) => Sem (FileLock ': r) a -> Sem r a
runFileLockIO = interpretH $ \case
  WithFileLock' p ma -> bracket (embed $ lockFile (toFilePath p) Exclusive) (embed . unlockFile) (const (runTSimple ma))
