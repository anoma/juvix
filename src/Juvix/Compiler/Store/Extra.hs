module Juvix.Compiler.Store.Extra where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language (TopModulePath)
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Internal.Data.Name
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Internal.Language
import Juvix.Compiler.Store.Language
import Juvix.Compiler.Store.Scoped.Data.InfoTable qualified as Scoped
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

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
mkModuleTable = ModuleTable . hashMap . map (\mi -> (getModulePath mi, mi))

lookupModule :: ModuleTable -> TopModulePath -> ModuleInfo
lookupModule mtab n = fromJust $ HashMap.lookup n (mtab ^. moduleTable)

insertModule :: TopModulePath -> ModuleInfo -> ModuleTable -> ModuleTable
insertModule p mi = over moduleTable (HashMap.insert p mi)

computeCombinedScopedInfoTable :: ModuleTable -> Scoped.InfoTable
computeCombinedScopedInfoTable mtab =
  mconcatMap (^. moduleInfoScopedModule . scopedModuleInfoTable) (HashMap.elems (mtab ^. moduleTable))

computeCombinedCoreInfoTable :: ModuleTable -> Core.InfoTable
computeCombinedCoreInfoTable mtab =
  mconcatMap (toCore . (^. moduleInfoCoreTable)) (HashMap.elems (mtab ^. moduleTable))

computeCombinedBuiltins :: ModuleTable -> HashMap BuiltinPrim Name
computeCombinedBuiltins mtab =
  mconcatMap
    (^. moduleInfoInternalModule . internalModuleInfoTable . infoBuiltins)
    (HashMap.elems (mtab ^. moduleTable))
