module Juvix.Compiler.Mono.Data.LocalVars where

import Juvix.Compiler.Mono.Language
import Juvix.Prelude

newtype LocalVars = LocalVars
  { _localTypes :: HashMap VarName Type
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''LocalVars
