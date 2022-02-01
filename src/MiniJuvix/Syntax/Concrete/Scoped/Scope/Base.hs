{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Syntax.Concrete.Scoped.Scope.Base (
 ScopeError,
 LocalVariable,
 SymbolInfo,
 ScopeSpace,
 ModuleNameId,
 ExportModuleSymbolInfo,
 WhyInScope,
 SymbolEntry(..),
 ExportScope,
 Scope,
 emptyScope
      ) where

import MiniJuvix.Utils.Prelude
import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified MiniJuvix.Syntax.Concrete.Name as C

data ScopeError
  = ErrParser Text
  | Err
  | ErrInfixParser String
  | ErrInfixPattern String
  | ErrAlreadyDefined Symbol
  | ErrLacksTypeSig Symbol
  | ErrImportCycle TopModulePath
  | ErrOpenNotInScope QualifiedName
  | ErrSymNotInScope Symbol
  | ErrQualSymNotInScope QualifiedName
  | ErrModuleNotInScope QualifiedName
  | ErrBindGroup Symbol
  | ErrDuplicateFixity Symbol
  | ErrMultipleExport Symbol
  | ErrAmbiguousSym [(S.AbsModulePath, SymbolEntry)]
  | ErrAmbiguousModuleSym [(S.AbsModulePath, SymbolEntry)]
  deriving stock (Show)


newtype LocalVariable = LocalVariable
  { variableName :: S.Symbol
  }
  deriving newtype (Show, Eq, Hashable)

newtype LocalVars = LocalVars
  { _localVars :: HashMap Symbol LocalVariable
  }

newtype SymbolInfo = SymbolInfo
  { -- | This map must have at least one entry. If there are more than one
    -- entry, it means that the same symbol has been brought into scope from two
    -- different places.
    _symbolInfo :: HashMap S.AbsModulePath SymbolEntry
  }
  deriving newtype (Show, Semigroup, Monoid)

data SpaceNode = SpaceFolder
  | SpaceModule ModuleNameId
  deriving stock (Show, Eq, Generic)
instance Hashable SpaceNode

newtype ScopeSpace = ScopeSpace
  { -- | TODO explain why
    _spaceChildren :: HashMap Symbol (HashMap SpaceNode ScopeSpace)
  }
  deriving stock (Show)

type ModuleNameId = S.NameId

data ExportModuleSymbolInfo = ExportModuleSymbolInfo
  {
    _exportModuleNameId :: S.NameId,
    _exportModuleSymbolPath :: S.AbsModulePath,
    _exportModuleSymbolScope :: ExportScope
  }
  deriving stock (Show)

-- | Why a symbol is in scope.
data WhyInScope =
  -- | Inherited from the parent module.
  BecauseInherited WhyInScope
  -- | Opened in this module.
  | BecauseOpened
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
data ExportScope = ExportScope {
   _exportSymbols :: HashMap Symbol SymbolEntry,
   _exportModules :: HashMap Symbol ExportModuleSymbolInfo
  }
  deriving stock (Show)
makeLenses ''ExportScope
makeLenses ''SymbolEntry
makeLenses ''SymbolInfo

data Scope = Scope
  { _scopePath :: S.AbsModulePath,
    _scopeFixities :: HashMap Symbol Fixity,
    _scopeSymbols :: HashMap Symbol SymbolInfo,
    _scopeSpace :: ScopeSpace,
    _scopeModules :: HashMap ModuleNameId ExportScope,
    _scopeBindGroup :: HashMap Symbol LocalVariable
  }
makeLenses ''Scope

emptyScope :: S.AbsModulePath -> Scope
emptyScope absPath = Scope
        { _scopePath = absPath,
          _scopeFixities = mempty,
          _scopeSymbols = mempty,
          _scopeSpace = ScopeSpace mempty,
          _scopeModules = mempty,
          _scopeBindGroup = mempty
        }

lookupModule :: forall r. Members '[State Scope] r =>
   ModuleNameId -> Sem r ExportScope  
lookupModule uid = fromMaybe impossible . HashMap.lookup uid <$> gets _scopeModules

lookupQualifiedSymbol :: forall r. Members '[State Scope, Error ScopeError] r =>
   QualifiedName -> Sem r [SymbolEntry]
lookupQualifiedSymbol = undefined
  where
  errAmbiguousModule :: [(S.AbsModulePath, SymbolEntry)] -> Sem r a
  errAmbiguousModule = throw . ErrAmbiguousModuleSym

lookupQualifiedSymbol' :: forall r. Members '[State Scope, Error ScopeError] r =>
   QualifiedName -> Sem r [SymbolEntry]
lookupQualifiedSymbol' q@(QualifiedName (Path path) sym) = case nonEmpty path of
  Nothing -> impossible
  Just ne -> gets _scopeSpace >>= findSymbol ne
  where
  errNotInScope :: Sem r a
  errNotInScope = throw (ErrQualSymNotInScope q)
  findSymbol :: NonEmpty Symbol -> ScopeSpace -> Sem r [SymbolEntry]
  findSymbol qual space = do
    modules <- gets _scopeModules
    mapMaybe (HashMap.lookup  sym . _exportSymbols) . mapMaybe (`HashMap.lookup` modules)
                  . toList <$> findSpace qual space
  findSpace :: NonEmpty Symbol -> ScopeSpace -> Sem r (HashSet ModuleNameId)
  findSpace (p :| ps) ScopeSpace{..} =
    case HashMap.lookup p _spaceChildren of
    Nothing -> errNotInScope
    Just m -> case HashMap.toList m of
      [] -> impossible
      l -> mconcatMapM (uncurry (goNode ps)) l
  goNode :: [Symbol] -> SpaceNode -> ScopeSpace -> Sem r (HashSet ModuleNameId)
  goNode syms node space = case nonEmpty syms of
    Nothing -> case node of
      SpaceFolder -> errNotInScope
      SpaceModule modId -> return $ HashSet.singleton modId
    Just syms' -> findSpace syms' space

      

unqualifiedSName :: S.Symbol -> S.Name
unqualifiedSName = over S.nameConcrete NameUnqualified

entryToSName :: s -> SymbolEntry -> S.Name' s
entryToSName s SymbolEntry {..} =
  S.Name'
    { _nameId = _symbolId,
      _nameConcrete = s,
      _nameDefinedIn = _symbolDefinedIn,
      _nameFixity = _symbolFixity,
      _nameKind = _symbolKind
    }

checkUnqualified ::
  Members '[Error ScopeError, State Scope, Reader LocalVars] r =>
  Symbol ->
  Sem r S.Name
checkUnqualified s = do
  -- Local vars have scope priority
  l <- HashMap.lookup s <$> asks _localVars
  case l of
    Just LocalVariable {..} -> return (unqualifiedSName variableName)
    Nothing -> do
      -- Lookup at the global scope
      let err = throw (ErrSymNotInScope s)
      SymbolInfo {..} <- fromMaybeM err (HashMap.lookup s <$> gets _scopeSymbols)
      case HashMap.toList _symbolInfo of
        [] -> impossible
        [(_, e)] -> return (entryToSName (NameUnqualified s) e)
        es -> throw (ErrAmbiguousSym es) -- This is meant to happen only at the top level
