module Juvix.Compiler.Store.Internal.Language
  ( module Juvix.Compiler.Store.Internal.Data.InfoTable,
    module Juvix.Compiler.Store.Internal.Language,
    module Juvix.Compiler.Store.Internal.Data.TypeCheckingTables,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Store.Internal.Data.CoercionInfo
import Juvix.Compiler.Store.Internal.Data.FunctionsTable
import Juvix.Compiler.Store.Internal.Data.InfoTable
import Juvix.Compiler.Store.Internal.Data.InstanceInfo
import Juvix.Compiler.Store.Internal.Data.TypeCheckingTables
import Juvix.Extra.Serialize
import Juvix.Prelude

data InternalModule = InternalModule
  { _internalModuleId :: ModuleId,
    _internalModuleName :: Name,
    _internalModuleImports :: [Import],
    _internalModuleInfoTable :: InfoTable,
    _internalModuleTypeCheckingTables :: TypeCheckingTables
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

computeTypeCheckingTables :: InternalModuleTable -> TypeCheckingTables
computeTypeCheckingTables itab =
  TypeCheckingTables
    { _typeCheckingTablesTypesTable = computeTypesTable,
      _typeCheckingTablesInstanceTable = computeInstanceTable,
      _typeCheckingTablesFunctionsTable = computeFunctionsTable,
      _typeCheckingTablesCoercionTable = computeCoercionTable
    }
  where
    computeTypesTable :: TypesTable
    computeTypesTable = mconcatMap (^. internalModuleTypeCheckingTables . typeCheckingTablesTypesTable) (itab ^. internalModuleTable)

    computeFunctionsTable :: FunctionsTable
    computeFunctionsTable =
      mconcatMap
        (^. internalModuleTypeCheckingTables . typeCheckingTablesFunctionsTable)
        (itab ^. internalModuleTable)

    computeInstanceTable :: InstanceTable
    computeInstanceTable = mconcatMap (^. internalModuleTypeCheckingTables . typeCheckingTablesInstanceTable) (itab ^. internalModuleTable)

    computeCoercionTable :: CoercionTable
    computeCoercionTable = mconcatMap (^. internalModuleTypeCheckingTables . typeCheckingTablesCoercionTable) (itab ^. internalModuleTable)
