module Juvix.Compiler.Store.Internal.Data.TypeCheckingTables
  ( module Juvix.Compiler.Store.Internal.Data.TypeCheckingTables,
    module Juvix.Compiler.Store.Internal.Data.CoercionInfo,
    module Juvix.Compiler.Store.Internal.Data.FunctionsTable,
    module Juvix.Compiler.Store.Internal.Data.InstanceInfo,
    module Juvix.Compiler.Store.Internal.Data.PolarityTable,
    module Juvix.Compiler.Store.Internal.Data.TypesTable,
  )
where

import Juvix.Compiler.Store.Internal.Data.CoercionInfo
import Juvix.Compiler.Store.Internal.Data.FunctionsTable
import Juvix.Compiler.Store.Internal.Data.InstanceInfo
import Juvix.Compiler.Store.Internal.Data.PolarityTable
import Juvix.Compiler.Store.Internal.Data.TypesTable
import Juvix.Extra.Serialize
import Juvix.Prelude

data TypeCheckingTables = TypeCheckingTables
  { _typeCheckingTablesTypesTable :: TypesTable,
    _typeCheckingTablesFunctionsTable :: FunctionsTable,
    _typeCheckingTablesPolarityTable :: PolarityTable,
    _typeCheckingTablesInstanceTable :: InstanceTable,
    _typeCheckingTablesCoercionTable :: CoercionTable
  }
  deriving stock (Generic)

makeLenses ''TypeCheckingTables

instance Serialize TypeCheckingTables

instance NFData TypeCheckingTables

instance Monoid TypeCheckingTables where
  mempty =
    TypeCheckingTables
      { _typeCheckingTablesTypesTable = mempty,
        _typeCheckingTablesFunctionsTable = mempty,
        _typeCheckingTablesInstanceTable = mempty,
        _typeCheckingTablesPolarityTable = mempty,
        _typeCheckingTablesCoercionTable = mempty
      }

instance Semigroup TypeCheckingTables where
  a <> b =
    TypeCheckingTables
      { _typeCheckingTablesTypesTable = mappendField' typeCheckingTablesTypesTable,
        _typeCheckingTablesFunctionsTable = mappendField' typeCheckingTablesFunctionsTable,
        _typeCheckingTablesPolarityTable = mappendField' typeCheckingTablesPolarityTable,
        _typeCheckingTablesInstanceTable = mappendField' typeCheckingTablesInstanceTable,
        _typeCheckingTablesCoercionTable = mappendField' typeCheckingTablesCoercionTable
      }
    where
      mappendField' :: (Semigroup f) => Lens' TypeCheckingTables f -> f
      mappendField' = mappendField a b
