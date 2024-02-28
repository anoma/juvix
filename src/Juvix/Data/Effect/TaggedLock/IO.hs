module Juvix.Data.Effect.TaggedLock.IO where

import Juvix.Data.Effect.FileLock
import Juvix.Data.Effect.Files
import Juvix.Data.Effect.TaggedLock.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path

-- | Interpret `TaggedLock` using `FileLock`.
--
-- When multiple processes or threads call `withTaggedLock` with the same tag,
-- then only one of them can perform the action at a time.
runTaggedLockIO :: forall r a. (Members '[EmbedIO] r) => Sem (TaggedLock ': r) a -> Sem r a
runTaggedLockIO sem = do
  rootLockPath <- (<//> $(mkRelDir "juvix-file-locks")) <$> getTempDir
  runFileLockIO (runFilesIO (go rootLockPath sem))
  where
    go :: Path Abs Dir -> Sem (TaggedLock ': r) a -> Sem (Files ': FileLock ': r) a
    go r = interpretTop2H $ \locEnv -> \case
      WithTaggedLock t ma -> do
        p <- normalizeFile (r <//> t)
        ensureDir' (parent p)
        withFileLock' p (runTSimpleEff locEnv ma)
