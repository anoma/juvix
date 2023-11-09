module Juvix.Data.ModuleId where

import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Prettyprinter

data ModuleId = ModuleId
  { _moduleIdPath :: Text,
    _moduleIdPackage :: Text,
    _moduleIdPackageVersion :: Text
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

makeLenses ''ModuleId

instance Pretty ModuleId where
  pretty ModuleId {..} = pretty _moduleIdPath

instance Hashable ModuleId

instance Serialize ModuleId

defaultModuleId :: ModuleId
defaultModuleId =
  ModuleId
    { _moduleIdPath = "$DefaultModule$",
      _moduleIdPackage = "$",
      _moduleIdPackageVersion = "1.0"
    }
