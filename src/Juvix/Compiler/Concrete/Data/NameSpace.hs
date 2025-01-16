module Juvix.Compiler.Concrete.Data.NameSpace where

import Data.Kind qualified as GHC
import Juvix.Compiler.Concrete.Data.Name qualified as C
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Data.VisibilityAnn
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Data.NameKind
import Juvix.Prelude

data NameSpace
  = NameSpaceSymbols
  | NameSpaceModules
  | NameSpaceFixities
  deriving stock (Eq, Generic, Enum, Bounded, Show, Ord)

instance Hashable NameSpace

$(genSingletons [''NameSpace])

type NameKindNameSpace :: NameKind -> NameSpace
type family NameKindNameSpace s = res where
  NameKindNameSpace 'KNameLocal = 'NameSpaceSymbols
  NameKindNameSpace 'KNameAlias = 'NameSpaceSymbols
  NameKindNameSpace 'KNameConstructor = 'NameSpaceSymbols
  NameKindNameSpace 'KNameInductive = 'NameSpaceSymbols
  NameKindNameSpace 'KNameFunction = 'NameSpaceSymbols
  NameKindNameSpace 'KNameAxiom = 'NameSpaceSymbols
  NameKindNameSpace 'KNameLocalModule = 'NameSpaceModules
  NameKindNameSpace 'KNameTopModule = 'NameSpaceModules
  NameKindNameSpace 'KNameFixity = 'NameSpaceFixities

type NameSpaceEntryType :: NameSpace -> GHC.Type
type family NameSpaceEntryType s = res | res -> s where
  NameSpaceEntryType 'NameSpaceSymbols = PreSymbolEntry
  NameSpaceEntryType 'NameSpaceModules = ModuleSymbolEntry
  NameSpaceEntryType 'NameSpaceFixities = FixitySymbolEntry

nameSpaceElemName :: (IsString str) => NameSpace -> str
nameSpaceElemName = \case
  NameSpaceSymbols -> "symbol"
  NameSpaceModules -> "module"
  NameSpaceFixities -> "fixity"

nsEntry :: forall ns. (SingI ns) => Lens' (NameSpaceEntryType ns) S.Name
nsEntry = case sing :: SNameSpace ns of
  SNameSpaceModules -> moduleEntry
  SNameSpaceSymbols -> preSymbolName
  SNameSpaceFixities -> fixityEntry

shouldExport :: (SingI ns) => NameSpaceEntryType ns -> Bool
shouldExport ent = ent ^. nsEntry . S.nameVisibilityAnn == VisPublic

forEachNameSpace :: (Monad m) => (forall (ns :: NameSpace). Sing ns -> m ()) -> m ()
forEachNameSpace f = sequence_ [withSomeSing ns f | ns <- allElements]

entryName :: forall ns. (SingI ns) => Lens' (NameSpaceEntryType ns) S.Name
entryName = case sing :: SNameSpace ns of
  SNameSpaceSymbols -> \f -> \case
    PreSymbolAlias (Alias n) -> PreSymbolAlias . Alias <$> f n
    PreSymbolFinal (SymbolEntry n) -> PreSymbolFinal . SymbolEntry <$> f n
  SNameSpaceModules -> moduleEntry
  SNameSpaceFixities -> fixityEntry

exportNameSpace :: forall ns. (SingI ns) => Lens' ExportInfo (HashMap C.Symbol (NameSpaceEntryType ns))
exportNameSpace = case sing :: SNameSpace ns of
  SNameSpaceSymbols -> exportSymbols
  SNameSpaceModules -> exportModuleSymbols
  SNameSpaceFixities -> exportFixitySymbols

resolveNameSpaceEntry :: forall ns. (SingI ns) => NameSpaceEntryType ns -> NameSpaceEntryType ns -> NameSpaceEntryType ns
resolveNameSpaceEntry = case sing :: SNameSpace ns of
  SNameSpaceSymbols -> resolvePreSymbolEntry
  SNameSpaceModules -> resolveModuleSymbolEntry
  SNameSpaceFixities -> resolveFixitySymbolEntry
  where
    resolvePreSymbolEntry :: PreSymbolEntry -> PreSymbolEntry -> PreSymbolEntry
    resolvePreSymbolEntry = \cases
      (PreSymbolAlias (Alias n1)) (PreSymbolAlias (Alias n2)) -> PreSymbolAlias (Alias (resolveName n1 n2))
      (PreSymbolFinal (SymbolEntry n1)) (PreSymbolFinal (SymbolEntry n2)) -> PreSymbolFinal (SymbolEntry (resolveName n1 n2))
      _ _ -> impossible

    resolveModuleSymbolEntry :: ModuleSymbolEntry -> ModuleSymbolEntry -> ModuleSymbolEntry
    resolveModuleSymbolEntry (ModuleSymbolEntry n1) (ModuleSymbolEntry n2) = ModuleSymbolEntry (resolveName n1 n2)

    resolveFixitySymbolEntry :: FixitySymbolEntry -> FixitySymbolEntry -> FixitySymbolEntry
    resolveFixitySymbolEntry (FixitySymbolEntry n1) (FixitySymbolEntry n2) = FixitySymbolEntry (resolveName n1 n2)

    resolveName :: S.Name -> S.Name -> S.Name
    resolveName n1 n2 =
      over S.nameVisibilityAnn (resolveVisibility (n2 ^. S.nameVisibilityAnn)) n1
