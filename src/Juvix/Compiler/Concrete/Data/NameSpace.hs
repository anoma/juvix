module Juvix.Compiler.Concrete.Data.NameSpace where

import Data.Kind qualified as GHC
import Juvix.Prelude

data NameSpace
  = NameSpaceSymbols
  | NameSpaceModules
  deriving stock (Eq)

type AnyNameSpace (k :: NameSpace -> GHC.Type) =
  Σ NameSpace (TyCon1 k)

$(genSingletons [''NameSpace])
