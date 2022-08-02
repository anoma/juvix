module Juvix.Compiler.Concrete.Data.VisibilityAnn where

import Juvix.Prelude

data VisibilityAnn
  = VisPublic
  | VisPrivate
  deriving stock (Show, Eq, Ord)
