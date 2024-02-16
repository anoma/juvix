module Juvix.Compiler.Concrete.Data.NameSpace where

import Data.Kind qualified as GHC
import Juvix.Compiler.Concrete.Data.Name qualified as C
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

exportNameSpace :: forall ns. (SingI ns) => Lens' ExportInfo (HashMap C.Symbol (NameSpaceEntryType ns))
exportNameSpace = case sing :: SNameSpace ns of
  SNameSpaceSymbols -> exportSymbols
  SNameSpaceModules -> exportModuleSymbols
  SNameSpaceFixities -> exportFixitySymbols

resolveNameSpaceEntry :: forall ns. (SingI ns) => NameSpaceEntryType ns -> NameSpaceEntryType ns -> NameSpaceEntryType ns
resolveNameSpaceEntry = case sing :: SNameSpace ns of
  SNameSpaceSymbols -> resolvePreSymbolEntry
  SNameSpaceModules -> resolveEntry
  SNameSpaceFixities -> resolveEntry
  where
    resolvePreSymbolEntry :: PreSymbolEntry -> PreSymbolEntry -> PreSymbolEntry
    resolvePreSymbolEntry = \cases
      (PreSymbolAlias (Alias n1)) (PreSymbolAlias (Alias n2)) -> PreSymbolAlias (Alias (resolveEntry n1 n2))
      (PreSymbolFinal n1) (PreSymbolFinal n2) -> PreSymbolFinal (resolveEntry n1 n2)
      _ _ -> impossible

    resolveEntry :: Entry k -> Entry k -> Entry k
    resolveEntry e1 e2
      | e1 ^. entryName == e2 ^. entryName =
          Entry
            { _entryName = e1 ^. entryName,
              _entryVisibility = resolveVisibility (e1 ^. entryVisibility) (e2 ^. entryVisibility)
            }
      | otherwise = impossible
