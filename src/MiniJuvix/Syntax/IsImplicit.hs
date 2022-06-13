module MiniJuvix.Syntax.IsImplicit where

import MiniJuvix.Prelude

data IsImplicit = Explicit | Implicit
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable IsImplicit
