module MiniJuvix.Syntax.MicroJuvix.LocalVars where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

newtype LocalVars = LocalVars
  { _localTypes :: HashMap VarName Type
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''LocalVars
