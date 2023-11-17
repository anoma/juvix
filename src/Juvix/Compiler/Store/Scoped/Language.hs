module Juvix.Compiler.Store.Scoped.Language where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Name qualified as C
import Juvix.Compiler.Concrete.Data.ScopedName (HasNameKind)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Extra.Serialize
import Juvix.Prelude

newtype Alias = Alias
  { _aliasName :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize Alias

-- | Either an alias or a symbol entry.
data PreSymbolEntry
  = PreSymbolAlias Alias
  | PreSymbolFinal SymbolEntry
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize PreSymbolEntry

-- | A symbol which is not an alias.
newtype SymbolEntry = SymbolEntry
  { _symbolEntry :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable SymbolEntry

instance Serialize SymbolEntry

newtype ModuleSymbolEntry = ModuleSymbolEntry
  { _moduleEntry :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize ModuleSymbolEntry

newtype FixitySymbolEntry = FixitySymbolEntry
  { _fixityEntry :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize FixitySymbolEntry

-- | Symbols that a module exports
data ExportInfo = ExportInfo
  { _exportSymbols :: HashMap C.Symbol PreSymbolEntry,
    _exportModuleSymbols :: HashMap C.Symbol ModuleSymbolEntry,
    _exportFixitySymbols :: HashMap C.Symbol FixitySymbolEntry
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize ExportInfo

data ScopedModule = ScopedModule
  { _scopedModulePath :: S.TopModulePath,
    _scopedModuleName :: S.Name,
    _scopedModuleFilePath :: Path Abs File,
    _scopedModuleExportInfo :: ExportInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize ScopedModule

makeLenses ''Alias
makeLenses ''SymbolEntry
makeLenses ''ModuleSymbolEntry
makeLenses ''FixitySymbolEntry
makeLenses ''ExportInfo
makeLenses ''ScopedModule

instance HasLoc Alias where
  getLoc = (^. aliasName . S.nameDefined)

instance HasLoc PreSymbolEntry where
  getLoc = \case
    PreSymbolAlias a -> getLoc a
    PreSymbolFinal a -> getLoc a

instance HasLoc SymbolEntry where
  getLoc = (^. symbolEntry . S.nameDefined)

instance HasNameKind ModuleSymbolEntry where
  getNameKind (ModuleSymbolEntry s) = S.getNameKind s

instance HasLoc ModuleSymbolEntry where
  getLoc (ModuleSymbolEntry s) = s ^. S.nameDefined

symbolEntryNameId :: SymbolEntry -> NameId
symbolEntryNameId = (^. symbolEntry . S.nameId)

instance HasNameKind SymbolEntry where
  getNameKind = S.getNameKind . (^. symbolEntry)

preSymbolName :: Lens' PreSymbolEntry S.Name
preSymbolName f = \case
  PreSymbolAlias a -> PreSymbolAlias <$> traverseOf aliasName f a
  PreSymbolFinal a -> PreSymbolFinal <$> traverseOf symbolEntry f a

exportAllNames :: SimpleFold ExportInfo S.Name
exportAllNames =
  exportSymbols
    . each
    . preSymbolName
    <> exportModuleSymbols
      . each
      . moduleEntry
    <> exportFixitySymbols
      . each
      . fixityEntry

createExportsTable :: ExportInfo -> HashSet NameId
createExportsTable = HashSet.fromList . (^.. exportAllNames . S.nameId)
