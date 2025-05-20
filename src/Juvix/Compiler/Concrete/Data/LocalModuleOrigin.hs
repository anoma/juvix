module Juvix.Compiler.Concrete.Data.LocalModuleOrigin where

import Juvix.Prelude

data LocalModuleOrigin
  = -- | The local module was in the source code
    LocalModuleSource
  | -- | The local module was inserted because of a type definition. The `Int`
    --  is the number of automatically inserted statements at the beginning of
    --  the module that should not be printed
    LocalModuleType Int
  | -- | The local module was inserted because of a public import
    LocalModulePublicImport
  deriving stock (Eq, Ord, Show, Generic, Data)

instance NFData LocalModuleOrigin

instance Serialize LocalModuleOrigin
