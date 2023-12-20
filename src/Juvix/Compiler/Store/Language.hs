module Juvix.Compiler.Store.Language where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language (TopModulePath)
import Juvix.Compiler.Store.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Store.Internal.Language
import Juvix.Compiler.Store.Options
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

data ModuleInfo = ModuleInfo
  { _moduleInfoScopedModule :: ScopedModule,
    _moduleInfoInternalModule :: InternalModule,
    _moduleInfoCoreTable :: Core.InfoTable,
    _moduleInfoImports :: [TopModulePath],
    _moduleInfoOptions :: Options,
    _moduleInfoSHA256 :: Text
  }
  deriving stock (Generic)

instance Serialize ModuleInfo

newtype ModuleTable = ModuleTable
  { _moduleTable :: HashMap TopModulePath ModuleInfo
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''ModuleInfo
makeLenses ''ModuleTable

getModulePath :: ModuleInfo -> TopModulePath
getModulePath mi = mi ^. moduleInfoScopedModule . scopedModulePath . S.nameConcrete

getModuleId :: ModuleInfo -> ModuleId
getModuleId mi = mi ^. moduleInfoScopedModule . scopedModuleId

getScopedModuleTable :: ModuleTable -> ScopedModuleTable
getScopedModuleTable mtab =
  ScopedModuleTable $ fmap (^. moduleInfoScopedModule) (mtab ^. moduleTable)

getInternalModuleTable :: ModuleTable -> InternalModuleTable
getInternalModuleTable mtab =
  InternalModuleTable $
    HashMap.fromList (map (\mi -> (mi ^. moduleInfoInternalModule . internalModuleName, mi ^. moduleInfoInternalModule)) (HashMap.elems (mtab ^. moduleTable)))

mkModuleTable :: [ModuleInfo] -> ModuleTable
mkModuleTable = ModuleTable . HashMap.fromList . map (\mi -> (getModulePath mi, mi))

lookupModule :: ModuleTable -> TopModulePath -> ModuleInfo
lookupModule mtab n = fromJust $ HashMap.lookup n (mtab ^. moduleTable)

insertModule :: TopModulePath -> ModuleInfo -> ModuleTable -> ModuleTable
insertModule p mi = over moduleTable (HashMap.insert p mi)