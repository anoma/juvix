module Juvix.Compiler.Internal.Data.TypedInstanceHole where

import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data TypedInstanceHole = TypedInstanceHole
  { _typedHoleHole :: InstanceHole,
    _typedHoleType :: Expression,
    _typedHoleLocalVars :: LocalVars
  }

makeLenses ''TypedInstanceHole
