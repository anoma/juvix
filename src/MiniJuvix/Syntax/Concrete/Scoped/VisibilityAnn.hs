module MiniJuvix.Syntax.Concrete.Scoped.VisibilityAnn where

import MiniJuvix.Prelude

data VisibilityAnn
  = VisPublic
  | VisPrivate
  deriving stock (Show, Eq, Ord)
