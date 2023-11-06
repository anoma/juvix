module Juvix.Compiler.Store.Scoped.Language where

import Juvix.Compiler.Concrete.Data.Name qualified as C
import Juvix.Compiler.Concrete.Data.ScopedName (HasNameKind)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Prelude

type ScopedName = S.Name

newtype Alias = Alias
  { _aliasName :: ScopedName
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Either an alias or a symbol entry.
data PreSymbolEntry
  = PreSymbolAlias Alias
  | PreSymbolFinal SymbolEntry
  deriving stock (Show, Eq, Ord, Generic)

-- | A symbol which is not an alias.
newtype SymbolEntry = SymbolEntry
  { _symbolEntry :: ScopedName
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable SymbolEntry

newtype ModuleSymbolEntry = ModuleSymbolEntry
  { _moduleEntry :: ScopedName
  }
  deriving stock (Show, Eq, Ord, Generic)

newtype FixitySymbolEntry = FixitySymbolEntry
  { _fixityEntry :: ScopedName
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Symbols that a module exports
data ExportInfo = ExportInfo
  { _exportSymbols :: HashMap C.Symbol PreSymbolEntry,
    _exportModuleSymbols :: HashMap C.Symbol ModuleSymbolEntry,
    _exportFixitySymbols :: HashMap C.Symbol FixitySymbolEntry
  }
  deriving stock (Show, Eq, Ord, Generic)

data ScopedModule = ScopedModule
  { _scopedModulePath :: S.TopModulePath,
    _scopedModuleName :: ScopedName,
    _scopedModuleExportInfo :: ExportInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

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

preSymbolName :: Lens' PreSymbolEntry ScopedName
preSymbolName f = \case
  PreSymbolAlias a -> PreSymbolAlias <$> traverseOf aliasName f a
  PreSymbolFinal a -> PreSymbolFinal <$> traverseOf symbolEntry f a

exportAllNames :: SimpleFold ExportInfo ScopedName
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
