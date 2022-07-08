module Juvix.Syntax.ForeignBlock where

import Juvix.Prelude
import Juvix.Syntax.Backends

data ForeignBlock = ForeignBlock
  { _foreignBackend :: Backend,
    _foreignCode :: Text
  }
  deriving stock (Eq, Ord, Show)

makeLenses ''ForeignBlock
