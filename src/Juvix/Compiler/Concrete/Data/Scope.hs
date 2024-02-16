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
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

nsEntryVisibility :: forall ns. (SingI ns) => Lens' (NameSpaceEntryType ns) VisibilityAnn
nsEntryVisibility = case sing :: SNameSpace ns of
  SNameSpaceModules -> entryVisibility
  SNameSpaceSymbols -> preSymbolEntryVisibility
  SNameSpaceFixities -> entryVisibility

nsEntry' :: forall ns. (SingI ns) => NameSpaceEntryType ns -> SomeEntry
nsEntry' = case sing :: SNameSpace ns of
  SNameSpaceModules -> SomeEntry
  SNameSpaceSymbols -> preSymbolEntryToSomeEntry
  SNameSpaceFixities -> SomeEntry

nsEntry :: forall ns. (SingI ns) => Lens' (NameSpaceEntryType ns) S.Name
nsEntry = case sing :: SNameSpace ns of
  SNameSpaceModules -> entryName
  SNameSpaceSymbols -> preSymbolName
  SNameSpaceFixities -> entryName

scopeNameSpace :: forall (ns :: NameSpace). (SingI ns) => Lens' Scope (HashMap Symbol (SymbolInfo ns))
scopeNameSpace = case sing :: SNameSpace ns of
  SNameSpaceSymbols -> scopeSymbols
  SNameSpaceModules -> scopeModuleSymbols
  SNameSpaceFixities -> scopeFixitySymbols

scopeNameSpaceLocal :: forall (ns :: NameSpace). Sing ns -> Lens' Scope (HashMap Symbol S.Symbol)
scopeNameSpaceLocal s = case s of
  SNameSpaceSymbols -> scopeLocalSymbols
  SNameSpaceModules -> scopeLocalModuleSymbols
  SNameSpaceFixities -> scopeLocalFixitySymbols

emptyScope :: S.AbsModulePath -> Scope
emptyScope absPath =
  Scope
    { _scopePath = absPath,
      _scopeSymbols = mempty,
      _scopeModuleSymbols = mempty,
      _scopeFixitySymbols = mempty,
      _scopeTopModules = mempty,
      _scopeLocalSymbols = mempty,
      _scopeLocalModuleSymbols = mempty,
      _scopeLocalFixitySymbols = mempty
    }
