module Juvix.Syntax.IsImplicit where

import Juvix.Prelude
import Juvix.Prelude.Pretty

data IsImplicit = Explicit | Implicit
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable IsImplicit

instance Pretty IsImplicit where
  pretty = \case
    Implicit -> "implicit"
    Explicit -> "explicit"
