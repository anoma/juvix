{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Syntax.Concrete.Scoped.Scope where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S

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

type SymbolEntry = S.Name' ()

-- data SymbolEntry = SymbolEntry
--   { _symbolKind :: S.NameKind,
--     _symbolDefinedIn :: S.AbsModulePath,
--     _symbolDefined :: Interval,
--     _symbolId :: S.NameId,
--     _symbolFixity :: S.NameFixity,
--     _symbolWhyInScope :: S.WhyInScope,
--     _symbolPublicAnn :: PublicAnn
--   }
--   deriving stock (Show)

-- getSymbolKind :: SymbolEntry -> S.NameKind
-- getSymbolKind SymbolEntry {..} = fromSing _symbolKind

-- instance HasLoc SymbolEntry where
--   getLoc SymbolEntry {..} = case _symbolKind of
--     S.SKNameTopModule -> getLoc _symbolSymbol
--     S.SKNameAxiom -> getLoc _symbolSymbol
--     S.SKNameConstructor -> getLoc _symbolSymbol
--     S.SKNameInductive -> getLoc _symbolSymbol
--     S.SKNameFunction -> getLoc _symbolSymbol
--     S.SKNameLocal -> getLoc _symbolSymbol
--     S.SKNameLocalModule -> getLoc _symbolSymbol

-- | Symbols that a module exports
newtype ExportInfo = ExportInfo {
   _exportSymbols :: HashMap Symbol SymbolEntry
  }

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
    _scopeFixities :: HashMap Symbol OperatorSyntaxDef,
    _scopeSymbols :: HashMap Symbol SymbolInfo,
    _scopeTopModules :: HashMap TopModulePath S.ModuleNameId,
    _scopeBindGroup :: HashMap Symbol LocalVariable
  }
 deriving stock (Show)
makeLenses ''ExportInfo
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
    _scopeTopParents :: [Import 'Parsed]
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
