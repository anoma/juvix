module Juvix.Data.Effect.FileLock.Permissive where

import Juvix.Data.Effect.FileLock.Base
import Juvix.Prelude

runFileLockPermissive :: Sem (FileLock ': r) a -> Sem r a
runFileLockPermissive = interpretH $ \case
  WithFileLockDir _ ma -> runTSimple ma
