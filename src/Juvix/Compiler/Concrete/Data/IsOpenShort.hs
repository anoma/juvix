module Juvix.Compiler.Concrete.Data.IsOpenShort where

import Juvix.Prelude

data IsOpenShort
  = OpenFull
  | OpenShort
  deriving stock (Eq, Ord, Show, Generic, Data, Enum, Bounded)

$(genSingletons [''IsOpenShort])
