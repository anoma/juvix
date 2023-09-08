module Juvix.Compiler.Internal.Data.TypedHole where

import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data TypedHole = TypedHole
  { _typedHoleHole :: Hole,
    _typedHoleType :: Expression
  }

makeLenses ''TypedHole
