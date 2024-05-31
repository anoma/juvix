module Juvix.Compiler.Store.Internal.Language
  ( module Juvix.Compiler.Store.Internal.Data.InfoTable,
    module Juvix.Compiler.Store.Internal.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Store.Internal.Data.CoercionInfo
import Juvix.Compiler.Store.Internal.Data.FunctionsTable
import Juvix.Compiler.Store.Internal.Data.InfoTable
import Juvix.Compiler.Store.Internal.Data.InstanceInfo
import Juvix.Compiler.Store.Internal.Data.TypesTable
import Juvix.Extra.Serialize
import Juvix.Prelude

data InternalModule = InternalModule
  { _internalModuleId :: ModuleId,
    _internalModuleName :: Name,
    _internalModuleImports :: [Import],
    _internalModuleInfoTable :: InfoTable,
    _internalModuleInstanceTable :: InstanceTable,
    _internalModuleCoercionTable :: CoercionTable,
    _internalModuleTypesTable :: TypesTable,
    _internalModuleFunctionsTable :: FunctionsTable
  }
  deriving stock (Generic)

instance Serialize InternalModule

instance NFData InternalModule

newtype InternalModuleTable = InternalModuleTable
  { _internalModuleTable :: HashMap Name InternalModule
  }
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

instance Serialize InternalModuleTable

instance NFData InternalModuleTable

makeLenses ''InternalModule
makeLenses ''InternalModuleTable

lookupInternalModule :: InternalModuleTable -> Name -> InternalModule
lookupInternalModule mtab n = fromJust $ HashMap.lookup n (mtab ^. internalModuleTable)

insertInternalModule :: InternalModuleTable -> InternalModule -> InternalModuleTable
insertInternalModule tab sm = over internalModuleTable (HashMap.insert (sm ^. internalModuleName) sm) tab

computeCombinedInfoTable :: InternalModuleTable -> InfoTable
computeCombinedInfoTable = mconcatMap (^. internalModuleInfoTable) . HashMap.elems . (^. internalModuleTable)

computeTypesTable :: InternalModuleTable -> TypesTable
computeTypesTable = mconcatMap (^. internalModuleTypesTable) . (^. internalModuleTable)

computeFunctionsTable :: InternalModuleTable -> FunctionsTable
computeFunctionsTable = mconcatMap (^. internalModuleFunctionsTable) . (^. internalModuleTable)

computeInstanceTable :: InternalModuleTable -> InstanceTable
computeInstanceTable = mconcatMap (^. internalModuleInstanceTable) . (^. internalModuleTable)

computeCoercionTable :: InternalModuleTable -> CoercionTable
computeCoercionTable = mconcatMap (^. internalModuleCoercionTable) . (^. internalModuleTable)
