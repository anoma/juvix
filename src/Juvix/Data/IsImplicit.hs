module Juvix.Data.IsImplicit where

import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data IsImplicit
  = Explicit
  | Implicit
  | ImplicitInstance
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable IsImplicit

instance Pretty IsImplicit where
  pretty = \case
    Implicit -> "implicit"
    ImplicitInstance -> "implicitInstance"
    Explicit -> "explicit"
