module Juvix.Compiler.Internal.Data.TypedHole where

import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data TypedHole = TypedHole
  { _typedHoleHole :: InstanceHole,
    _typedHoleType :: Expression,
    _typedHoleLocalVars :: LocalVars
  }

makeLenses ''TypedHole
