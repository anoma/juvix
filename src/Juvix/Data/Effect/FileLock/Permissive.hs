module Juvix.Data.Effect.FileLock.Permissive where

import Juvix.Data.Effect.FileLock.Base
import Juvix.Prelude.Base

-- | Interpret `FileLock` by executing all actions unconditionally
runFileLockPermissive :: Sem (FileLock ': r) a -> Sem r a
runFileLockPermissive = interpretH $ \case
  WithFileLock' _ ma -> runTSimple ma
