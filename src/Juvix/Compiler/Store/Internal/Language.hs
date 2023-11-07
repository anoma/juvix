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

data StoredModule = StoredModule
  { _storedModuleName :: Name,
    _storedModuleImports :: [Import],
    _storedModuleInfoTable :: InfoTable
  }
  deriving stock (Generic)

instance Serialize StoredModule

newtype StoredModuleTable = StoredModuleTable
  { _storedModuleTable :: HashMap Name StoredModule
  }
  deriving stock (Generic)

instance Serialize StoredModuleTable

makeLenses ''StoredModule
makeLenses ''StoredModuleTable

lookupStoredModule :: StoredModuleTable -> Name -> StoredModule
lookupStoredModule mtab n = fromJust $ HashMap.lookup n (mtab ^. storedModuleTable)
