module MiniJuvix.Syntax.Concrete.BindingSite where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language

data BindingSite (s :: Stage) =
  BindConstructorDef (InductiveConstructorDef s)
  | BindPattern (PatternType s)
