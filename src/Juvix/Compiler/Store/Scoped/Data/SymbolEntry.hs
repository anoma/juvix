module Juvix.Compiler.Store.Scoped.Data.SymbolEntry where

import Juvix.Compiler.Concrete.Data.ScopedName (HasNameKind)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Extra.Serialize
import Juvix.Prelude

newtype Alias = Alias
  { _aliasName :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize Alias

instance NFData Alias

instance Hashable Alias

-- | Either an alias or a symbol entry.
data PreSymbolEntry
  = PreSymbolAlias Alias
  | PreSymbolFinal SymbolEntry
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize PreSymbolEntry

instance NFData PreSymbolEntry

instance Hashable PreSymbolEntry

-- | A symbol which is not an alias.
newtype SymbolEntry = SymbolEntry
  { _symbolEntry :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable SymbolEntry

instance Serialize SymbolEntry

instance NFData SymbolEntry

newtype ModuleSymbolEntry = ModuleSymbolEntry
  { _moduleEntry :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize ModuleSymbolEntry

instance NFData ModuleSymbolEntry

instance Hashable ModuleSymbolEntry

newtype FixitySymbolEntry = FixitySymbolEntry
  { _fixityEntry :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize FixitySymbolEntry

instance NFData FixitySymbolEntry

instance Hashable FixitySymbolEntry

makeLenses ''Alias
makeLenses ''SymbolEntry
makeLenses ''ModuleSymbolEntry
makeLenses ''FixitySymbolEntry

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
  getNameKindPretty (ModuleSymbolEntry s) = S.getNameKindPretty s

instance HasLoc ModuleSymbolEntry where
  getLoc (ModuleSymbolEntry s) = s ^. S.nameDefined

instance HasLoc FixitySymbolEntry where
  getLoc (FixitySymbolEntry s) = s ^. S.nameDefined

symbolEntryNameId :: SymbolEntry -> NameId
symbolEntryNameId = (^. symbolEntry . S.nameId)

instance HasNameKind SymbolEntry where
  getNameKind = S.getNameKind . (^. symbolEntry)
  getNameKindPretty = S.getNameKindPretty . (^. symbolEntry)

preSymbolName :: Lens' PreSymbolEntry S.Name
preSymbolName f = \case
  PreSymbolAlias a -> PreSymbolAlias <$> traverseOf aliasName f a
  PreSymbolFinal a -> PreSymbolFinal <$> traverseOf symbolEntry f a
