module Juvix.Syntax.MonoJuvix.LocalVars where

import Juvix.Prelude
import Juvix.Syntax.MonoJuvix.Language

newtype LocalVars = LocalVars
  { _localTypes :: HashMap VarName Type
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''LocalVars
