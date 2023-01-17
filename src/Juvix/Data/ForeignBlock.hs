module Juvix.Data.ForeignBlock where

import Juvix.Data.Backends
import Juvix.Prelude.Base

data ForeignBlock = ForeignBlock
  { _foreignBackend :: Backend,
    _foreignCode :: Text
  }
  deriving stock (Eq, Ord, Show, Data)

makeLenses ''ForeignBlock
