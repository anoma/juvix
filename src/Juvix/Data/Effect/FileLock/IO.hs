module Juvix.Data.Effect.FileLock.IO where

import Juvix.Data.Effect.FileLock.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import System.FileLock hiding (FileLock)

-- | Interpret `FileLock` using `System.FileLock`
runFileLockIO :: (Members '[EmbedIO] r) => Sem (FileLock ': r) a -> Sem r a
runFileLockIO = interpretH $ \locEnv -> \case
  WithFileLock' p ma -> bracket (liftIO (lockFile (toFilePath p) Exclusive)) (liftIO . unlockFile) (const (runTSimpleEff locEnv ma))
