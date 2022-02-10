{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Syntax.Concrete.Scoped.Scope where

import MiniJuvix.Utils.Prelude
import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S

data ScopeError
  = ErrParser Text
  | ErrGeneric Text
  | ErrInfixParser String
  | ErrInfixPattern String
  | ErrAlreadyDefined Symbol
  | ErrLacksTypeSig Symbol
  | ErrImportCycle TopModulePath
  | ErrOpenNotInScope QualifiedName
  | ErrSymNotInScope Symbol Scope LocalVars
  | ErrQualSymNotInScope QualifiedName
  | ErrModuleNotInScope Name
  | ErrBindGroup Symbol
  | ErrDuplicateFixity Symbol
  | ErrMultipleExport Symbol
  | ErrAmbiguousSym [SymbolEntry]
  | ErrAmbiguousModuleSym [SymbolEntry]
  deriving stock (Show)


newtype LocalVariable = LocalVariable
  { variableName :: S.Symbol
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

-- | Why a symbol is in scope.
data WhyInScope =
  -- | Inherited from the parent module.
  BecauseInherited WhyInScope
  -- | Opened or imported in this module.
  | BecauseImportedOpened
  -- | Defined in this module.
  | BecauseDefined
  deriving stock (Show)

data SymbolEntry = SymbolEntry
  { _symbolKind :: S.NameKind,
    _symbolDefinedIn :: S.AbsModulePath,
    _symbolId :: S.NameId,
    _symbolFixity :: S.NameFixity,
    _symbolWhyInScope :: WhyInScope,
    _symbolPublicAnn :: PublicAnn
  }
  deriving stock (Show)

-- | Symbols that a module exports
newtype ExportInfo = ExportInfo {
   _exportSymbols :: HashMap Symbol SymbolEntry
  }
  deriving stock (Show)

-- | A module entry for either a local or a top module.
type ModuleEntry = Σ ModuleIsTop (TyCon1 ModuleEntry')

mkModuleEntry :: SingI t => ModuleEntry' t -> ModuleEntry
mkModuleEntry = (sing :&:)

data ModuleEntry' (t :: ModuleIsTop) = ModuleEntry' {
  _moduleEntryExport :: ExportInfo,
  _moduleEntryScoped :: Module 'Scoped t
  }

data Scope = Scope
  { _scopePath :: S.AbsModulePath,
    _scopeFixities :: HashMap Symbol Fixity,
    _scopeSymbols :: HashMap Symbol SymbolInfo,
    _scopeTopModules :: HashMap TopModulePath S.ModuleNameId,
    _scopeBindGroup :: HashMap Symbol LocalVariable
  }
  deriving stock (Show)
makeLenses ''ExportInfo
makeLenses ''SymbolEntry
makeLenses ''SymbolInfo
makeLenses ''LocalVars
makeLenses ''Scope
makeLenses ''ModuleEntry'

newtype ModulesCache = ModulesCache
  { _cachedModules :: HashMap TopModulePath (ModuleEntry' 'ModuleTop)
  }

makeLenses ''ModulesCache

data ScopeParameters = ScopeParameters
  { -- | Root of the project.
    _scopeRootPath :: FilePath,
    -- | Usually set to ".mjuvix".
    _scopeFileExtension :: String,
    -- | Used for import cycle detection.
    _scopeTopParents :: HashSet TopModulePath
  }
makeLenses ''ScopeParameters

data ScoperState = ScoperState
  { _scoperModulesCache :: ModulesCache,
    _scoperFreeNames :: Stream S.NameId,
    _scoperModules :: HashMap S.ModuleNameId ModuleEntry
  }
makeLenses ''ScoperState

emptyScope :: S.AbsModulePath -> Scope
emptyScope absPath = Scope
        { _scopePath = absPath,
          _scopeFixities = mempty,
          _scopeSymbols = mempty,
          _scopeTopModules = mempty,
          _scopeBindGroup = mempty
        }
