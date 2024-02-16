module Juvix.Compiler.Store.Scoped.Data.SymbolEntry
  ( module Juvix.Compiler.Store.Scoped.Data.SymbolEntry,
    module Juvix.Compiler.Concrete.Data.VisibilityAnn,
  )
where

import Juvix.Compiler.Concrete.Data.ScopedName (HasNameKind)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Data.VisibilityAnn
import Juvix.Extra.Serialize
import Juvix.Prelude

newtype Alias = Alias
  { _aliasEntry :: AliasEntry
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize Alias

-- | Either an alias or a symbol entry.
data PreSymbolEntry
  = PreSymbolAlias Alias
  | PreSymbolFinal SymbolEntry
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize PreSymbolEntry

data EntryKind
  = -- | An alias for a symbol in the symbols name space
    EntryKindSymbolAlias
  | -- | A symbol which is not an alias.
    EntryKindSymbol
  | EntryKindModule
  | EntryKindFixity

type SymbolEntry = Entry 'EntryKindSymbol

type AliasEntry = Entry 'EntryKindSymbolAlias

type ModuleSymbolEntry = Entry 'EntryKindModule

type FixitySymbolEntry = Entry 'EntryKindFixity

data SomeEntry = forall (k :: EntryKind).
  SomeEntry
  { _someEntry :: Entry k
  }

data Entry (k :: EntryKind) = Entry
  { _entryVisibility :: VisibilityAnn,
    _entryName :: S.Name
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize (Entry k)

makeLenses ''Alias
makeLenses ''SomeEntry
makeLenses ''Entry

instance HasLoc SomeEntry where
  getLoc (SomeEntry e) = getLoc e

instance HasLoc Alias where
  getLoc = (^. aliasEntry . entryName . S.nameDefined)

instance HasLoc PreSymbolEntry where
  getLoc = \case
    PreSymbolAlias a -> getLoc a
    PreSymbolFinal a -> getLoc a

instance HasLoc (Entry k) where
  getLoc = (^. entryName . S.nameDefined)

instance HasNameKind (Entry k) where
  getNameKind Entry {..} = S.getNameKind _entryName

symbolEntryNameId :: Entry k -> NameId
symbolEntryNameId = (^. entryName . S.nameId)

preSymbolName :: Lens' PreSymbolEntry S.Name
preSymbolName f = \case
  PreSymbolAlias a -> PreSymbolAlias <$> (aliasEntry . entryName) f a
  PreSymbolFinal a -> PreSymbolFinal <$> entryName f a

preSymbolEntryVisibility :: Lens' PreSymbolEntry VisibilityAnn
preSymbolEntryVisibility f = \case
  PreSymbolAlias a -> PreSymbolAlias <$> (aliasEntry . entryVisibility) f a
  PreSymbolFinal a -> PreSymbolFinal <$> entryVisibility f a

preSymbolEntryToSomeEntry :: PreSymbolEntry -> SomeEntry
preSymbolEntryToSomeEntry = \case
  PreSymbolAlias (Alias a) -> SomeEntry a
  PreSymbolFinal a -> SomeEntry a

someEntryVisibility :: Lens' SomeEntry VisibilityAnn
someEntryVisibility f (SomeEntry e) = SomeEntry <$> entryVisibility f e
