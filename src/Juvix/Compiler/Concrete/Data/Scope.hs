module Juvix.Compiler.Concrete.Data.Scope
  ( module Juvix.Compiler.Concrete.Data.Scope,
    module Juvix.Compiler.Concrete.Data.InfoTable,
  )
where

import Juvix.Compiler.Concrete.Data.InfoTable
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

newtype LocalVariable = LocalVariable
  { _variableName :: S.Symbol
  }
  deriving newtype (Show, Eq, Hashable)

newtype LocalVars = LocalVars
  { _localVars :: HashMap Symbol LocalVariable
  }
  deriving stock (Show)

newtype SymbolInfo = SymbolInfo
  { -- | This map must have at least one entry. If there are more than one
    -- entry, it means that the same symbol has been brought into scope from two
    -- different places.
    _symbolInfo :: HashMap S.AbsModulePath SymbolEntry
  }
  deriving newtype (Show, Semigroup, Monoid)

mkModuleRef' :: (SingI t) => ModuleRef'' 'S.NotConcrete t -> ModuleRef' 'S.NotConcrete
mkModuleRef' m = ModuleRef' (sing :&: m)

data Scope = Scope
  { _scopePath :: S.AbsModulePath,
    _scopeFixities :: HashMap Symbol OperatorSyntaxDef,
    _scopeSymbols :: HashMap Symbol SymbolInfo,
    _scopeTopModules :: HashMap TopModulePath (ModuleRef'' 'S.NotConcrete 'ModuleTop),
    -- | Symbols that have been defined in the current scope level. Every symbol
    -- should map to itself. This is needed because we may query it with a
    -- symbol with a different location but we may want the location of the
    -- original symbol
    _scopeLocalSymbols :: HashMap Symbol S.Symbol,
    _scopeBindGroup :: HashMap Symbol LocalVariable,
    _scopeCompilationRules :: HashMap Symbol CompileInfo
  }
  deriving stock (Show)

makeLenses ''ExportInfo
makeLenses ''LocalVariable
makeLenses ''SymbolInfo
makeLenses ''LocalVars
makeLenses ''Scope

newtype ModulesCache = ModulesCache
  { _cachedModules :: HashMap TopModulePath (ModuleRef'' 'S.NotConcrete 'ModuleTop)
  }

makeLenses ''ModulesCache

data ScopeParameters = ScopeParameters
  { -- | Used for import cycle detection.
    _scopeTopParents :: [Import 'Parsed],
    _scopeParsedModules :: HashMap TopModulePath (Module 'Parsed 'ModuleTop)
  }

makeLenses ''ScopeParameters

data ScoperState = ScoperState
  { _scoperModulesCache :: ModulesCache,
    _scoperModules :: HashMap S.ModuleNameId (ModuleRef' 'S.NotConcrete),
    _scoperScope :: HashMap TopModulePath Scope
  }

makeLenses ''ScoperState

emptyScope :: S.AbsModulePath -> Scope
emptyScope absPath =
  Scope
    { _scopePath = absPath,
      _scopeFixities = mempty,
      _scopeSymbols = mempty,
      _scopeTopModules = mempty,
      _scopeLocalSymbols = mempty,
      _scopeBindGroup = mempty,
      _scopeCompilationRules = mempty
    }
