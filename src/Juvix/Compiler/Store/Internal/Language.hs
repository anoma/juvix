module Juvix.Compiler.Store.Internal.Language
  ( module Juvix.Compiler.Store.Internal.Data.InfoTable,
    module Juvix.Compiler.Store.Internal.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Store.Internal.Data.InfoTable
import Juvix.Extra.Serialize
import Juvix.Prelude

data InternalModule = InternalModule
  { _internalModuleName :: Name,
    _internalModuleImports :: [Import],
    _internalModuleInfoTable :: InfoTable
  }
  deriving stock (Generic)

instance Serialize InternalModule

newtype InternalModuleTable = InternalModuleTable
  { _internalModuleTable :: HashMap Name InternalModule
  }
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

instance Serialize InternalModuleTable

makeLenses ''InternalModule
makeLenses ''InternalModuleTable

lookupInternalModule :: InternalModuleTable -> Name -> InternalModule
lookupInternalModule mtab n = fromJust $ HashMap.lookup n (mtab ^. internalModuleTable)

insertInternalModule :: InternalModuleTable -> InternalModule -> InternalModuleTable
insertInternalModule tab sm = over internalModuleTable (HashMap.insert (sm ^. internalModuleName) sm) tab
