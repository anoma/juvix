module Juvix.Compiler.Store.Language where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language (TopModulePath)
import Juvix.Compiler.Store.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Store.Internal.Language
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

data ModuleInfo = ModuleInfo
  { _moduleInfoScopedModule :: ScopedModule,
    _moduleInfoStoredModule :: StoredModule,
    _moduleInfoCoreTable :: Core.InfoTable
  }

newtype ModuleTable = ModuleTable
  { _moduleTable :: HashMap TopModulePath ModuleInfo
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''ModuleInfo
makeLenses ''ModuleTable

getModulePath :: ModuleInfo -> TopModulePath
getModulePath mi = mi ^. moduleInfoScopedModule . scopedModulePath . S.nameConcrete

getStoredModuleTable :: ModuleTable -> StoredModuleTable
getStoredModuleTable mtab =
  StoredModuleTable $
    HashMap.fromList (map (\mi -> (mi ^. moduleInfoStoredModule . storedModuleName, mi ^. moduleInfoStoredModule)) (HashMap.elems (mtab ^. moduleTable)))

mkModuleTable :: [ModuleInfo] -> ModuleTable
mkModuleTable = ModuleTable . HashMap.fromList . map (\mi -> (getModulePath mi, mi))
