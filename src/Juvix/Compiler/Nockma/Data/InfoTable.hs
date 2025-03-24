module Juvix.Compiler.Nockma.Data.InfoTable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude hiding (Path)

data InfoTable = InfoTable
  { _infoFunctions :: HashMap Symbol FunctionInfo,
    -- | Transitive imports.
    _infoImports :: HashSet ModuleId,
    -- | Unquoted code.
    _infoCode :: Maybe (Term Natural)
  }
  deriving stock (Generic)

data FunctionInfo = FunctionInfo
  { _functionInfoName :: Text,
    -- | Path within the function's module.
    _functionInfoPath :: Path,
    _functionInfoModuleId :: ModuleId,
    _functionInfoArity :: Natural
  }
  deriving stock (Generic)

makeLenses ''InfoTable
makeLenses ''FunctionInfo

instance Serialize FunctionInfo

instance Serialize InfoTable

instance Semigroup InfoTable where
  t1 <> t2 =
    InfoTable
      { _infoFunctions = t1 ^. infoFunctions <> t2 ^. infoFunctions,
        _infoImports = t1 ^. infoImports <> t2 ^. infoImports,
        _infoCode = Nothing
      }

instance Monoid InfoTable where
  mempty =
    InfoTable
      { _infoFunctions = mempty,
        _infoImports = mempty,
        _infoCode = Nothing
      }

lookupTabFunInfo' :: InfoTable -> Symbol -> Maybe FunctionInfo
lookupTabFunInfo' InfoTable {..} sym = HashMap.lookup sym _infoFunctions

lookupTabFunInfo :: InfoTable -> Symbol -> FunctionInfo
lookupTabFunInfo tab sym = fromJust (lookupTabFunInfo' tab sym)
