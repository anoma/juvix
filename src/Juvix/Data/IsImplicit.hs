module Juvix.Data.IsImplicit where

import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data IsImplicit
  = Explicit
  | Implicit
  | ImplicitInstance
  deriving stock (Show, Eq, Ord, Generic, Data)

isImplicitOrInstance :: IsImplicit -> Bool
isImplicitOrInstance = \case
  Explicit -> False
  Implicit -> True
  ImplicitInstance -> True

instance Hashable IsImplicit

instance Pretty IsImplicit where
  pretty = \case
    Implicit -> "implicit"
    ImplicitInstance -> "implicit instance"
    Explicit -> "explicit"
