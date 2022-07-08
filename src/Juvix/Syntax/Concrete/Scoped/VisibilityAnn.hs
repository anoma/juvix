module Juvix.Syntax.Concrete.Scoped.VisibilityAnn where

import Juvix.Prelude

data VisibilityAnn
  = VisPublic
  | VisPrivate
  deriving stock (Show, Eq, Ord)
