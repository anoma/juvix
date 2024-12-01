module Juvix.Data.ModuleId where

import Juvix.Data.TopModulePathKey
import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Prettyprinter

data ModuleId = ModuleId
  { _moduleIdPath :: TopModulePathKey,
    _moduleIdPackageName :: Text,
    _moduleIdPackageVersion :: Text
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

makeLenses ''ModuleId

instance Pretty ModuleId where
  pretty ModuleId {..} = pretty _moduleIdPath

instance Hashable ModuleId

instance Serialize ModuleId

instance NFData ModuleId

defaultModuleId :: ModuleId
defaultModuleId =
  ModuleId
    { _moduleIdPath = nonEmptyToTopModulePathKey (pure "$DefaultModule$"),
      _moduleIdPackageName = "$",
      _moduleIdPackageVersion = "1.0"
    }
