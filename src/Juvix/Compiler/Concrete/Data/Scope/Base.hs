module Juvix.Compiler.Concrete.Data.Scope.Base where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.NameSpace
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

newtype SymbolInfo (n :: NameSpace) = SymbolInfo
  { -- | This map must have at least one entry. If there are more than one
    -- entry, it means that the same symbol has been brought into scope from two
    -- different places
    _symbolInfo :: HashMap S.AbsModulePath (NameSpaceEntryType n)
  }

instance (SingI n) => Semigroup (SymbolInfo n) where
  SymbolInfo s1 <> SymbolInfo s2 =
    SymbolInfo (HashMap.unionWith resolveNameSpaceEntry s1 s2)

instance (SingI n) => Monoid (SymbolInfo n) where
  mempty = SymbolInfo mempty

data BindingStrategy
  = -- | Local binding allows shadowing
    BindingLocal
  | -- | Top binding does not allow shadowing. It may result in an ambiguous error
    BindingTop

data InScope = InScope
  { _inScopeSymbols :: HashMap Symbol (SymbolInfo 'NameSpaceSymbols),
    -- | Local module symbols (excluding top modules associated with files)
    _inScopeLocalModuleSymbols :: HashMap Symbol (SymbolInfo 'NameSpaceModules),
    _inScopeFixitySymbols :: HashMap Symbol (SymbolInfo 'NameSpaceFixities)
  }

-- | Symbols that have been defined in the current scope level. Every symbol
-- should map to itself. This is needed because we may query it with a
-- symbol with a different location but we may want the location of the
-- original symbol
data Reserved = Reserved
  { _reservedLocalSymbols :: HashMap Symbol S.Symbol,
    _reservedLocalModuleSymbols :: HashMap Symbol S.Symbol,
    _reservedLocalFixitySymbols :: HashMap Symbol S.Symbol
  }

data Scope = Scope
  { _scopePath :: S.AbsModulePath,
    _scopeInScope :: InScope,
    -- | The map from S.NameId to Modules is needed because we support merging
    -- several imports under the same name. E.g.
    -- import A as X;
    -- import B as X;
    _scopeImports :: HashMap TopModulePathKey (HashMap S.NameId ScopedModule),
    _scopeReserved :: Reserved
  }

newtype ModulesCache = ModulesCache
  { _cachedModules :: HashMap TopModulePathKey ScopedModule
  }

newtype ScopeParameters = ScopeParameters
  { _scopeImportedModules :: HashMap TopModulePathKey ScopedModule
  }

data ReservedModule = ReservedModule
  { _reservedModuleName :: S.Name,
    _reservedModuleExportInfo :: ExportInfo,
    _reservedModuleReserved :: Reserved,
    _reservedModuleStatements :: StatementSections 'Parsed
  }

data ScoperState = ScoperState
  { -- | Local and top modules currently in scope - used to look up qualified symbols
    -- TODO unify
    _scoperModules :: HashMap S.NameId ScopedModule,
    _scoperReservedModules :: HashMap S.NameId ReservedModule,
    _scoperAlias :: HashMap S.NameId PreSymbolEntry,
    _scoperNameSignatures :: HashMap S.NameId (NameSignature 'Parsed),
    -- | Indexed by the inductive type. This is used for record updates
    _scoperRecordFields :: HashMap S.NameId RecordInfo,
    -- | Indexed by constructor. This is used for record patterns
    _scoperConstructorFields :: HashMap S.NameId (RecordNameSignature 'Parsed),
    _scoperScopedConstructorFields :: HashMap S.NameId (RecordNameSignature 'Scoped)
  }

data SymbolOperator = SymbolOperator
  { _symbolOperatorUsed :: Bool,
    _symbolOperatorFixity :: Fixity,
    _symbolOperatorDef :: OperatorSyntaxDef 'Parsed
  }
  deriving stock (Show)

newtype ScoperOperators = ScoperOperators
  { _scoperOperators :: HashMap Symbol SymbolOperator
  }
  deriving newtype (Semigroup, Monoid)

data SymbolIterator = SymbolIterator
  { _symbolIteratorUsed :: Bool,
    _symbolIteratorDef :: IteratorSyntaxDef 'Parsed
  }

newtype ScoperIterators = ScoperIterators
  { _scoperIterators :: HashMap Symbol SymbolIterator
  }
  deriving newtype (Semigroup, Monoid)

data ScoperSyntax = ScoperSyntax
  { _scoperSyntaxOperators :: ScoperOperators,
    _scoperSyntaxIterators :: ScoperIterators
  }

emptyScoperSyntax :: ScoperSyntax
emptyScoperSyntax = ScoperSyntax mempty mempty

makeLenses ''ScoperIterators
makeLenses ''InScope
makeLenses ''ReservedModule
makeLenses ''SymbolOperator
makeLenses ''SymbolIterator
makeLenses ''SymbolInfo
makeLenses ''Scope
makeLenses ''ScoperOperators
makeLenses ''ScoperSyntax
makeLenses ''ScoperState
makeLenses ''ScopeParameters
makeLenses ''ModulesCache
makeLenses ''Reserved

scopedToReservedModule :: ScopedModule -> ReservedModule
scopedToReservedModule scoped =
  ReservedModule
    { _reservedModuleName = scoped ^. scopedModuleName,
      _reservedModuleExportInfo = scoped ^. scopedModuleExportInfo,
      _reservedModuleReserved = emptyReserved,
      _reservedModuleStatements = SectionsEmpty
    }

emptyReserved :: Reserved
emptyReserved =
  Reserved
    { _reservedLocalSymbols = mempty,
      _reservedLocalModuleSymbols = mempty,
      _reservedLocalFixitySymbols = mempty
    }

emptyInScope :: InScope
emptyInScope =
  InScope
    { _inScopeSymbols = mempty,
      _inScopeLocalModuleSymbols = mempty,
      _inScopeFixitySymbols = mempty
    }

emptyScope :: S.AbsModulePath -> Scope
emptyScope absPath =
  Scope
    { _scopePath = absPath,
      _scopeInScope = emptyInScope,
      _scopeImports = mempty,
      _scopeReserved = emptyReserved
    }

scopeNameSpace :: forall (ns :: NameSpace). (SingI ns) => Lens' Scope (HashMap Symbol (SymbolInfo ns))
scopeNameSpace = case sing :: SNameSpace ns of
  SNameSpaceSymbols -> scopeSymbols
  SNameSpaceModules -> scopeModuleSymbols
  SNameSpaceFixities -> scopeFixitySymbols

reservedNameSpace ::
  forall (ns :: NameSpace).
  Sing ns ->
  Lens' Reserved (HashMap Symbol S.Symbol)
reservedNameSpace = \case
  SNameSpaceSymbols -> reservedLocalSymbols
  SNameSpaceModules -> reservedLocalModuleSymbols
  SNameSpaceFixities -> reservedLocalFixitySymbols

scopeSymbols :: Lens' Scope (HashMap Symbol (SymbolInfo 'NameSpaceSymbols))
scopeSymbols = scopeInScope . inScopeSymbols

scopeModuleSymbols :: Lens' Scope (HashMap Symbol (SymbolInfo 'NameSpaceModules))
scopeModuleSymbols = scopeInScope . inScopeLocalModuleSymbols

scopeFixitySymbols :: Lens' Scope (HashMap Symbol (SymbolInfo 'NameSpaceFixities))
scopeFixitySymbols = scopeInScope . inScopeFixitySymbols

scopeReservedSymbols :: Lens' Scope (HashMap Symbol S.Symbol)
scopeReservedSymbols = scopeReserved . reservedLocalSymbols

scopeReservedLocalModuleSymbols :: Lens' Scope (HashMap Symbol S.Symbol)
scopeReservedLocalModuleSymbols = scopeReserved . reservedLocalModuleSymbols

scopeReservedFixitySymbols :: Lens' Scope (HashMap Symbol S.Symbol)
scopeReservedFixitySymbols = scopeReserved . reservedLocalFixitySymbols
