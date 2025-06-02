module Juvix.Data.ModuleId where

import Juvix.Data.PackageId
import Juvix.Data.TopModulePathKey
import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Prettyprinter

data ModuleId = ModuleId
  { _moduleIdPath :: TopModulePathKey,
    _moduleIdPackageId :: PackageId
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
      _moduleIdPackageId =
        PackageId
          { _packageIdName = "$",
            _packageIdVersion = SemVer 1 0 0 Nothing Nothing
          }
    }

getModuleId :: forall r. (Member (Reader PackageId) r) => TopModulePathKey -> Sem r ModuleId
getModuleId path = do
  pkgId <- ask
  return
    ModuleId
      { _moduleIdPath = path,
        _moduleIdPackageId = pkgId
      }
