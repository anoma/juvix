module MiniJuvix.Syntax.MonoJuvix.LocalVars where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MonoJuvix.Language

newtype LocalVars = LocalVars
  { _localTypes :: HashMap VarName Type
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''LocalVars
