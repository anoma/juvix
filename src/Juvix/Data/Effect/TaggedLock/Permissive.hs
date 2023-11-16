module Juvix.Data.Effect.TaggedLock.Permissive where

import Juvix.Data.Effect.TaggedLock.Base
import Juvix.Prelude

runTaggedLockPermissive :: Sem (TaggedLock ': r) a -> Sem r a
runTaggedLockPermissive = interpretH $ \case
  WithTaggedLock _ ma -> runTSimple ma
