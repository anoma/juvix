module Juvix.Compiler.Concrete.Data.Scope.Base where

import Juvix.Compiler.Concrete.Data.NameSignature.Base
import Juvix.Compiler.Concrete.Data.NameSpace
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

newtype SymbolInfo (n :: NameSpace) = SymbolInfo
  { -- | This map must have at least one entry. If there are more than one
    -- entry, it means that the same symbol has been brought into scope from two
    -- different places
    _symbolInfo :: HashMap S.AbsModulePath (NameSpaceEntryType n)
  }
  deriving newtype (Semigroup, Monoid)

data BindingStrategy
  = -- | Local binding allows shadowing
    BindingLocal
  | -- | Top binding does not allow shadowing. It may result in an ambiguous error
    BindingTop

data Scope = Scope
  { _scopePath :: S.AbsModulePath,
    _scopeSymbols :: HashMap Symbol (SymbolInfo 'NameSpaceSymbols),
    _scopeModuleSymbols :: HashMap Symbol (SymbolInfo 'NameSpaceModules),
    -- | The map from S.NameId to Modules is needed because we support merging
    -- several imports under the same name. E.g.
    -- import A as X;
    -- import B as X;
    _scopeTopModules :: HashMap TopModulePath (HashMap S.NameId (ModuleRef'' 'S.NotConcrete 'ModuleTop)),
    -- | Symbols that have been defined in the current scope level. Every symbol
    -- should map to itself. This is needed because we may query it with a
    -- symbol with a different location but we may want the location of the
    -- original symbol
    _scopeLocalSymbols :: HashMap Symbol S.Symbol,
    _scopeLocalModuleSymbols :: HashMap Symbol S.Symbol
  }

newtype ModulesCache = ModulesCache
  { _cachedModules :: HashMap TopModulePath (ModuleRef'' 'S.NotConcrete 'ModuleTop)
  }

data ScopeParameters = ScopeParameters
  { -- | Used for import cycle detection.
    _scopeTopParents :: [Import 'Parsed],
    _scopeParsedModules :: HashMap TopModulePath (Module 'Parsed 'ModuleTop)
  }

data RecordInfo = RecordInfo
  { _recordInfoConstructor :: S.Symbol,
    _recordInfoSignature :: RecordNameSignature
  }

data ScoperState = ScoperState
  { _scoperModulesCache :: ModulesCache,
    -- | Local and top modules
    _scoperModules :: HashMap S.ModuleNameId (ModuleRef' 'S.NotConcrete),
    _scoperScope :: HashMap TopModulePath Scope,
    _scoperSignatures :: HashMap S.NameId NameSignature,
    -- | Indexed by the inductive type. This is meant to be used for record updates
    _scoperRecordSignatures :: HashMap S.NameId RecordInfo
  }

data SymbolFixity = SymbolFixity
  { _symbolFixityUsed :: Bool,
    _symbolFixityDef :: OperatorSyntaxDef
  }

newtype ScoperFixities = ScoperFixites
  { _scoperFixities :: HashMap Symbol SymbolFixity
  }
  deriving newtype (Semigroup, Monoid)

data SymbolIterator = SymbolIterator
  { _symbolIteratorUsed :: Bool,
    _symbolIteratorDef :: IteratorSyntaxDef
  }

newtype ScoperIterators = ScoperIterators
  { _scoperIterators :: HashMap Symbol SymbolIterator
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''ScoperIterators
makeLenses ''SymbolIterator
makeLenses ''SymbolInfo
makeLenses ''Scope
makeLenses ''ScoperFixities
makeLenses ''SymbolFixity
makeLenses ''ScoperState
makeLenses ''ScopeParameters
makeLenses ''ModulesCache
makeLenses ''RecordInfo
