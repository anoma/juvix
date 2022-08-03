module Juvix.Data.IsImplicit where

import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data IsImplicit = Explicit | Implicit
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable IsImplicit

instance Pretty IsImplicit where
  pretty = \case
    Implicit -> "implicit"
    Explicit -> "explicit"
