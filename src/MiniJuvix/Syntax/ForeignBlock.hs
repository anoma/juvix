module MiniJuvix.Syntax.ForeignBlock where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Backends

data ForeignBlock = ForeignBlock
  { _foreignBackend :: Backend,
    _foreignCode :: Text
  }
  deriving stock (Eq, Ord, Show)


makeLenses ''ForeignBlock
