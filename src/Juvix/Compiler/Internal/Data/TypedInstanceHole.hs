module Juvix.Compiler.Internal.Data.TypedInstanceHole where

import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data TypedInstanceHole = TypedInstanceHole
  { _typedInstanceHoleHole :: InstanceHole,
    _typedInstanceHoleType :: Expression,
    _typedInstanceHoleLocalVars :: LocalVars
  }

makeLenses ''TypedInstanceHole
