module Juvix.Compiler.Concrete.Data.ModuleIsTop where

import Juvix.Prelude

data ModuleIsTop = ModuleTop | ModuleLocal
  deriving stock (Eq, Ord, Show)

$(genSingletons [''ModuleIsTop])
