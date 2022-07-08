module Juvix.Syntax.Concrete.Scoped.Scope
  ( module Juvix.Syntax.Concrete.Scoped.Scope,
    module Juvix.Syntax.Concrete.Scoped.InfoTable,
  )
where

import Juvix.Prelude
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Scoped.InfoTable
import Juvix.Syntax.Concrete.Scoped.Name qualified as S

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

mkModuleRef' :: SingI t => ModuleRef'' 'S.NotConcrete t -> ModuleRef' 'S.NotConcrete
mkModuleRef' m = ModuleRef' (sing :&: m)

data Scope = Scope
  { _scopePath :: S.AbsModulePath,
    _scopeFixities :: HashMap Symbol OperatorSyntaxDef,
    _scopeSymbols :: HashMap Symbol SymbolInfo,
    _scopeTopModules :: HashMap TopModulePath (ModuleRef'' 'S.NotConcrete 'ModuleTop),
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
  { -- | Root of the project.
    _scopeRootPath :: FilePath,
    -- | Usually set to ".juvix".
    _scopeFileExtension :: String,
    -- | Used for import cycle detection.
    _scopeTopParents :: [Import 'Parsed]
  }

makeLenses ''ScopeParameters

data ScoperState = ScoperState
  { _scoperModulesCache :: ModulesCache,
    _scoperModules :: HashMap S.ModuleNameId (ModuleRef' 'S.NotConcrete)
  }

makeLenses ''ScoperState

emptyScope :: S.AbsModulePath -> Scope
emptyScope absPath =
  Scope
    { _scopePath = absPath,
      _scopeFixities = mempty,
      _scopeSymbols = mempty,
      _scopeTopModules = mempty,
      _scopeBindGroup = mempty,
      _scopeCompilationRules = mempty
    }
