module Juvix.Data.Effect.FileLock.Permissive where

import Juvix.Data.Effect.FileLock.Base
import Juvix.Prelude.Base

-- | Interpret `FileLock` by executing all actions unconditionally
runFileLockPermissive :: forall r a. Sem (FileLock ': r) a -> Sem r a
runFileLockPermissive = interpretH handler
  where
    handler ::
      forall x (localEs :: [Effect]).
      LocalEnv localEs r ->
      FileLock (Sem localEs) x ->
      Sem r x
    handler locEnv = \case
      WithFileLock' _ ma -> runTSimpleEff locEnv ma
