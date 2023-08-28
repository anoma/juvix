module Juvix.Compiler.Concrete.Data.Scope
  ( module Juvix.Compiler.Concrete.Data.Scope,
    module Juvix.Compiler.Concrete.Data.InfoTable,
    module Juvix.Compiler.Concrete.Data.NameSpace,
    module Juvix.Compiler.Concrete.Data.Scope.Base,
  )
where

import Juvix.Compiler.Concrete.Data.InfoTable
import Juvix.Compiler.Concrete.Data.NameSpace
import Juvix.Compiler.Concrete.Data.Scope.Base
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

nsEntry :: forall ns. (SingI ns) => Lens' (NameSpaceEntryType ns) (S.Name' ())
nsEntry = case sing :: SNameSpace ns of
  SNameSpaceModules -> moduleEntry
  SNameSpaceSymbols -> symbolEntry
  SNameSpaceFixities -> fixityEntry

mkModuleRef' :: (SingI t) => ModuleRef'' 'S.NotConcrete t -> ModuleRef' 'S.NotConcrete
mkModuleRef' m = ModuleRef' (sing :&: m)

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
