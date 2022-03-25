module MiniJuvix.Syntax.Concrete.Scoped.Scope where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import qualified Data.HashMap.Strict as HashMap

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

mkModuleRef' :: SingI t => ModuleRef'' 'S.NotConcrete t -> ModuleRef' 'S.NotConcrete
mkModuleRef' m = ModuleRef' (sing :&: m)

data Scope = Scope
  { _scopePath :: S.AbsModulePath,
    _scopeFixities :: HashMap Symbol OperatorSyntaxDef,
    _scopeSymbols :: HashMap Symbol SymbolInfo,
    _scopeTopModules :: HashMap TopModulePath (ModuleRef'' 'S.NotConcrete 'ModuleTop),
    _scopeBindGroup :: HashMap Symbol LocalVariable
  }
  deriving stock (Show)


newtype FunctionInfo = FunctionInfo {
  _functionInfoType :: Expression
  }

newtype ConstructorInfo = ConstructorInfo {
  _constructorInfoType :: Expression
  }

data AxiomInfo = AxiomInfo {
  _axiomInfoType :: Expression,
  _axiomInfoBackends :: [BackendItem]
  }

newtype InductiveInfo = InductiveInfo {
  _inductiveInfoDef :: InductiveDef 'Scoped
  }

data InfoTable = InfoTable {
  _infoConstructors :: HashMap ConstructorRef ConstructorInfo,
  _infoAxioms :: HashMap AxiomRef AxiomInfo,
  _infoInductives :: HashMap InductiveRef InductiveInfo,
  _infoFunctions :: HashMap FunctionRef FunctionInfo
  }

instance Semigroup InfoTable where
  (<>) = undefined
instance Monoid InfoTable where
  mempty = undefined

makeLenses ''ExportInfo
makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''FunctionInfo
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
    -- | Usually set to ".mjuvix".
    _scopeFileExtension :: String,
    -- | Used for import cycle detection.
    _scopeTopParents :: [Import 'Parsed]
  }

makeLenses ''ScopeParameters

data ScoperState = ScoperState
  { _scoperModulesCache :: ModulesCache,
    _scoperFreeNames :: Stream S.NameId,
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
      _scopeBindGroup = mempty
    }

emptyInfoTable :: InfoTable
emptyInfoTable = InfoTable {
  _infoConstructors = mempty,
  _infoAxioms = mempty,
  _infoInductives = mempty,
  _infoFunctions = mempty
  }
