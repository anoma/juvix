module Juvix.Compiler.Concrete.Data.NameSpace where

import Data.Kind qualified as GHC
import Juvix.Data.NameKind
import Juvix.Prelude

data NameSpace
  = NameSpaceSymbols
  | NameSpaceModules
  | NameSpaceFixities
  deriving stock (Eq, Generic, Enum, Bounded, Show, Ord)

instance Hashable NameSpace

type AnyNameSpace (k :: NameSpace -> GHC.Type) =
  Σ NameSpace (TyCon1 k)

$(genSingletons [''NameSpace])

type NameKindNameSpace :: NameKind -> NameSpace
type family NameKindNameSpace s = res where
  NameKindNameSpace 'KNameLocal = 'NameSpaceSymbols
  NameKindNameSpace 'KNameConstructor = 'NameSpaceSymbols
  NameKindNameSpace 'KNameInductive = 'NameSpaceSymbols
  NameKindNameSpace 'KNameFunction = 'NameSpaceSymbols
  NameKindNameSpace 'KNameAxiom = 'NameSpaceSymbols
  NameKindNameSpace 'KNameLocalModule = 'NameSpaceModules
  NameKindNameSpace 'KNameTopModule = 'NameSpaceModules
  NameKindNameSpace 'KNameFixity = 'NameSpaceFixities
