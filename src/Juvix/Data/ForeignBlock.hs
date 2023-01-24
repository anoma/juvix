module Juvix.Data.ForeignBlock where

import Juvix.Data.Backends
import Juvix.Data.Keyword
import Juvix.Data.WithLoc
import Juvix.Prelude.Base

data ForeignBlock = ForeignBlock
  { _foreignKw :: KeywordRef,
    _foreignBackend :: WithLoc Backend,
    _foreignCode :: Text
  }
  deriving stock (Eq, Ord, Show, Data)

makeLenses ''ForeignBlock
