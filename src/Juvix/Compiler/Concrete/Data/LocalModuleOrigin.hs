module Juvix.Compiler.Concrete.Data.LocalModuleOrigin where

import Juvix.Prelude

data LocalModuleOrigin
  = -- | The local module was in the source code
    LocalModuleSource
  | -- | The local module was inserted because of a type definition
    LocalModuleType
  | -- | The local module was inserted because of a public import
    LocalModulePublicImport
  deriving stock (Eq, Ord, Show, Generic, Data, Enum, Bounded)

instance NFData LocalModuleOrigin

instance Serialize LocalModuleOrigin
