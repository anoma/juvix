module Juvix.Syntax.IsImplicit where

import Juvix.Prelude

data IsImplicit = Explicit | Implicit
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable IsImplicit
