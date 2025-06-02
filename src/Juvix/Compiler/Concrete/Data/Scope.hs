module Juvix.Compiler.Concrete.Data.Scope
  ( module Juvix.Compiler.Concrete.Data.Scope,
    module Juvix.Compiler.Store.Scoped.Data.InfoTable,
    module Juvix.Compiler.Concrete.Data.NameSpace,
    module Juvix.Compiler.Concrete.Data.Scope.Base,
  )
where

import Juvix.Compiler.Concrete.Data.NameSpace
import Juvix.Compiler.Concrete.Data.Scope.Base
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Store.Scoped.Data.InfoTable
import Juvix.Prelude

scopeReservedNameSpace :: forall (ns :: NameSpace). Sing ns -> Lens' Scope (HashMap Symbol S.Symbol)
scopeReservedNameSpace s = case s of
  SNameSpaceSymbols -> scopeReservedSymbols
  SNameSpaceModules -> scopeReservedLocalModuleSymbols
  SNameSpaceFixities -> scopeReservedFixitySymbols
