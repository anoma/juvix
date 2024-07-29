module Juvix.Data.IsImplicit where

import Juvix.Extra.Serialize
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

instance Serialize IsImplicit

instance NFData IsImplicit

instance Pretty IsImplicit where
  pretty = \case
    Implicit -> "implicit"
    ImplicitInstance -> "implicit instance"
    Explicit -> "explicit"

-- | When we support Implicit record fields we should remove this type in favour
-- of IsImplicit
data IsImplicitField
  = ExplicitField
  | ImplicitInstanceField
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable IsImplicitField

instance Serialize IsImplicitField

instance NFData IsImplicitField

fromIsImplicitField :: IsImplicitField -> IsImplicit
fromIsImplicitField = \case
  ImplicitInstanceField -> ImplicitInstance
  ExplicitField -> Explicit

instance Pretty IsImplicitField where
  pretty = pretty . fromIsImplicitField
