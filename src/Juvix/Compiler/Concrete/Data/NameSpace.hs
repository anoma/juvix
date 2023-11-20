module Juvix.Compiler.Concrete.Data.NameSpace where

import Data.Kind qualified as GHC
import Juvix.Compiler.Concrete.Data.Name qualified as C
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
