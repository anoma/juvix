module Juvix.Compiler.Concrete.Data.NameRef where

import Data.Kind qualified as GHC
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S

type RefNameType :: S.IsConcrete -> GHC.Type
type family RefNameType c = res | res -> c where
  RefNameType 'S.Concrete = S.Name
  RefNameType 'S.NotConcrete = S.Name' ()
