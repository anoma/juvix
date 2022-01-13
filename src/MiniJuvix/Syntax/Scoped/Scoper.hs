{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module MiniJuvix.Syntax.Scoped.Scoper where

--------------------------------------------------------------------------------

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Lens.Micro.Platform
import MiniJuvix.Syntax.Concrete.Base (MonadParsec)
import qualified MiniJuvix.Syntax.Concrete.Base as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Parser (runModuleParserIO)
import qualified MiniJuvix.Syntax.Scoped.Name as S
import MiniJuvix.Utils.Prelude hiding (Reader, State, ask, asks, get, gets, local, modify, put)
import Polysemy
import Polysemy.Error hiding (fromEither)
import Polysemy.Reader
import Polysemy.State
import System.FilePath

--------------------------------------------------------------------------------

-- | Relevant scope information of a module.
data ModuleScopeInfo = ModuleScopeInfo
  {
    -- | Absolute path to the module
    _syntaxPath :: S.AbsModulePath,
    -- | Operator definitions. They can refer to either constructors or functions.
    _syntaxOperators :: HashMap Symbol Fixity,
    -- | constructors introduced by inductive definitions (E.g. zero; suc).
    _syntaxConstructors :: HashSet (DataConstructorName 'Parsed),
    -- | data types  introduced by inductive definitions (E.g. ℕ).
    _syntaxDataTypes :: HashSet (DataTypeName 'Parsed),
    -- | function names in scope. Function names are introduced with function clauses.
    _syntaxFunctions :: HashSet (FunctionName 'Parsed),
    -- | locally defined modules. Imported modules are not included.
    _syntaxLocalModules :: HashMap (LocalModuleName 'Parsed) ModuleScopeInfo
  }

newtype IdentifierInfo = IdentifierInfo
  { idenInfoOrigins :: HashSet TopModulePath
  }

newtype LocalVariable = LocalVariable
  { variableName :: S.Symbol
  }
  deriving newtype (Show, Eq, Hashable)

data SymbolInfo = SymbolInfo {
  -- | This map must have at least one entry.
  -- If there are more than one entry, it means that the same symbol has been
  -- brought into scope from two different places.
  symbolInfo :: HashMap S.AbsModulePath SymbolEntry
  }

data SymbolEntry = SymbolEntry {
  symbolKind :: S.NameKind,
  symbolDefinedIn :: S.AbsModulePath,
  symbolId :: S.NameId
  }
  deriving stock (Show)

data Scope = Scope
  {
    _scopePath      :: S.AbsModulePath,
    _scopeOperators :: HashMap Symbol Fixity,
    _scopeSymbols   :: HashMap Symbol SymbolInfo,
    _scopeModules   :: HashMap QualifiedName ModuleScopeInfo,
    _scopeBindGroup :: HashMap Symbol LocalVariable,
    _scopeLocalVars :: HashMap Symbol LocalVariable
  }

makeLenses ''Scope

newtype ModulesCache = ModulesCache
  { _cachedModules :: HashMap TopModulePath (Module 'Scoped 'ModuleTop)
  }

makeLenses ''ModulesCache

data ScopeError
  = ParseError Text
  | Err
  | ErrImportCycle TopModulePath
  | ErrOpenNotInScope QualifiedName
  | ErrSymNotInScope Symbol
  | ErrBindGroup Symbol
  | ErrAmbiguousSym [(S.AbsModulePath, SymbolEntry)]
  deriving stock (Show)

data ScopeParameters = ScopeParameters
  { -- | Root of the project.
    _scopeRootPath :: FilePath,
    -- | Usually set to ".mjuvix".
    _scopeFileExtension :: String,
    -- | Used for import cycle detection.
    _scopeTopParents :: HashSet TopModulePath
  }

makeLenses ''ScopeParameters

data ScopeState = ScopeState
  { _scopeModulesCacheCache :: ModulesCache,
    _scopeFreeNames :: Stream S.NameId
  }
makeLenses ''ScopeState

scopeCheck :: FilePath -> [Module 'Parsed 'ModuleTop] -> Either ScopeError [Module 'Scoped 'ModuleTop]
scopeCheck = undefined

freshNameId :: Members '[State ScopeState] r =>
  Sem r S.NameId
freshNameId = do
  i <- gets (Stream.head . _scopeFreeNames)
  modify (over scopeFreeNames Stream.tail)
  return i

freshVariable :: Members '[State ScopeState, State Scope] r => s -> Sem r (S.Name' s)
freshVariable = freshName S.KNameLocal

freshName :: Members '[State ScopeState, State Scope] r => S.NameKind -> s -> Sem r (S.Name' s)
freshName _nameKind _nameConcrete = do
  _nameId <- freshNameId
  _nameDefinedIn <- gets _scopePath
  return S.Name' {..}

checkImport ::
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, Embed IO, State ModulesCache] r =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImport (Import p) = Import <$> (readParseModule p >>= checkTopModule)

getTopModulePath :: Module s 'ModuleTop -> S.AbsModulePath
getTopModulePath Module{..} = S.AbsModulePath {
 S.absTopModulePath = moduleModulePath,
 S.absLocalPath = mempty
  }


localModuleScopeInfo :: S.AbsModulePath -> Module 'Scoped 'ModuleLocal -> ModuleScopeInfo
localModuleScopeInfo parentPath lmod = moduleScopeInfo (parentPath S.<.> localModSym) lmod
  where
  localModSym :: Symbol
  localModSym = S._nameConcrete (moduleModulePath lmod)

topModuleScopeInfo :: Module 'Scoped 'ModuleTop -> ModuleScopeInfo
topModuleScopeInfo m = moduleScopeInfo (getTopModulePath m) m

moduleScopeInfo :: S.AbsModulePath -> Module 'Scoped t -> ModuleScopeInfo
moduleScopeInfo modulePath (Module _ stmts) = ModuleScopeInfo {..}
  where
    _syntaxPath :: S.AbsModulePath
    _syntaxPath = undefined
    _syntaxLocalModules :: HashMap Symbol ModuleScopeInfo
    _syntaxLocalModules = HashMap.fromList (mapMaybe getModule stmts)
      where
        getModule :: Statement 'Scoped -> Maybe (Symbol, ModuleScopeInfo)
        getModule s = case s of
          StatementModule m@Module {..} -> Just (S._nameConcrete moduleModulePath, moduleScopeInfo undefined m)
          _ -> Nothing
    _syntaxFunctions :: HashSet (FunctionName 'Parsed)
    _syntaxFunctions = HashSet.fromList (mapMaybe getFun stmts)
      where
        getFun :: Statement 'Scoped -> Maybe (FunctionName 'Parsed)
        getFun s = case s of
          -- StatementDataType DataTypeDef {..} → HashSet.fromList (map constructorName dataTypeConstructors)
          _ -> undefined
    _syntaxConstructors :: HashSet (DataConstructorName 'Parsed)
    _syntaxConstructors = mconcat (map getConstrs stmts)
      where
        getConstrs :: Statement 'Scoped -> HashSet (DataConstructorName 'Parsed)
        getConstrs s = case s of
          StatementDataType DataTypeDef {..} ->
            HashSet.fromList
              (map (S._nameConcrete . constructorName) dataTypeConstructors)
          _ -> mempty
    _syntaxDataTypes :: HashSet (DataTypeName 'Parsed)
    _syntaxDataTypes = HashSet.fromList (mapMaybe getDT stmts)
      where
        getDT :: Statement 'Scoped -> Maybe (DataTypeName 'Parsed)
        getDT s = case s of
          StatementDataType DataTypeDef {..} -> Just (S._nameConcrete dataTypeName)
          _ -> Nothing
    _syntaxOperators :: HashMap Symbol Fixity
    _syntaxOperators = HashMap.fromList (mapMaybe getDef stmts)
      where
        getDef s = case s of
          StatementOperator OperatorSyntaxDef {..} -> Just (opSymbol, opFixity)
          _ -> Nothing

readParseModule ::
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO] r =>
  TopModulePath ->
  Sem r (Module 'Parsed 'ModuleTop)
readParseModule mp = do
  path <- modulePathToFilePath mp
  res <- embed (runModuleParserIO path)
  case res of
    Left err -> throw (ParseError err)
    Right r -> return r

modulePathToFilePath ::
  Members '[Reader ScopeParameters] r =>
  TopModulePath ->
  Sem r FilePath
modulePathToFilePath mp = do
  root <- asks _scopeRootPath
  ext <- asks _scopeFileExtension
  let relDirPath = foldr ((</>) . toPath) mempty (pathParts (modulePathDir mp))
      relFilePath = relDirPath </> toPath (modulePathName mp) <.> ext
  return $ root </> relFilePath
  where
    toPath :: Symbol -> FilePath
    toPath (Sym t) = Text.unpack t

addOperatorSyntaxDef ::
  Members '[Error ScopeError, State Scope] r =>
  OperatorSyntaxDef ->
  Sem r ()
addOperatorSyntaxDef = undefined

checkTypeSignature ::
  Members '[Error ScopeError, State Scope] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature = undefined

checkDataTypeDef ::
  Members '[Error ScopeError, State Scope] r =>
  DataTypeDef 'Parsed ->
  Sem r (DataTypeDef 'Scoped)
checkDataTypeDef _  = undefined

checkTopModule ::
  forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ModulesCache, Embed IO] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule (Module path stmts) = do
  checkCycle
  cache <- gets @ModulesCache _cachedModules
  maybe checkedModule return (cache ^. at path)
  where
    addParent :: ScopeParameters -> ScopeParameters
    addParent = over scopeTopParents (HashSet.insert path)
    checkedModule :: Sem r (Module 'Scoped 'ModuleTop)
    checkedModule =
      Module path
        <$> local addParent (mapM checkStatement stmts)
    checkCycle :: Sem r ()
    checkCycle =
      whenM
        (HashSet.member path <$> asks _scopeTopParents)
        (throw (ErrImportCycle path))

checkLocalModule ::
  forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ModulesCache, Embed IO] r =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule (Module path stmts) = Module undefined <$> mapM checkStatement stmts

checkOpenModule ::
  forall r.
  Members '[Error ScopeError, State Scope] r =>
  OpenModule ->
  Sem r ()
checkOpenModule OpenModule {..} = do
  info <- openInfo
  openModule openModuleName info openUsingHiding
  where
    openInfo :: Sem r ModuleScopeInfo
    openInfo = do
      r <- HashMap.lookup openModuleName <$> gets _scopeModules
      case r of
        Just info -> return info
        _ -> throw (ErrOpenNotInScope openModuleName)

openOperatorSyntaxDef ::
  forall r.
  Members '[Error ScopeError, State Scope] r =>
  QualifiedName ->
  (Symbol, Fixity) ->
  Sem r ()
openOperatorSyntaxDef path d = undefined

openModule ::
  forall r.
  Members '[Error ScopeError, State Scope] r =>
  QualifiedName ->
  ModuleScopeInfo ->
  Maybe UsingHiding ->
  Sem r ()
openModule qname ModuleScopeInfo {..} uh = do
  mapM_ (openOperatorSyntaxDef qname) (filter (shouldOpen . fst) (HashMap.toList _syntaxOperators))
  undefined
  where
    setsUsingHiding :: Maybe (Either (HashSet Symbol) (HashSet Symbol))
    setsUsingHiding = case uh of
      Just (Using l) -> Just (Left (HashSet.fromList (toList l)))
      Just (Hiding l) -> Just (Right (HashSet.fromList (toList l)))
      Nothing -> Nothing
    shouldOpen :: Symbol -> Bool
    shouldOpen s = case setsUsingHiding of
      Nothing -> True
      Just (Left using) -> HashSet.member s using
      Just (Right hiding) -> not (HashSet.member s hiding)

checkFunctionClause ::
  Members '[Error ScopeError, State Scope] r =>
  FunctionClause 'Parsed ->
  Sem r (FunctionClause 'Scoped)
checkFunctionClause = undefined

checkAxiom ::
  Members '[Error ScopeError, State Scope] r =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiom = undefined

checkEval ::
  Members '[Error ScopeError, State Scope] r =>
  Eval 'Parsed ->
  Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> checkParseExpressionSections s

checkPrint ::
  Members '[Error ScopeError, State Scope] r =>
  Print 'Parsed ->
  Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> checkParseExpressionSections s

checkFunction ::
  Members '[Error ScopeError, State Scope] r =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction = undefined

checkLetBlock ::
  Members '[Error ScopeError, State Scope] r =>
  LetBlock 'Parsed ->
  Sem r (LetBlock 'Scoped)
checkLetBlock = undefined

checkLambda ::
  Members '[Error ScopeError, State Scope] r =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda = undefined

checkQualified ::
  Members '[Error ScopeError, State Scope] r =>
  QualifiedName -> Sem r S.Name
checkQualified q = undefined

unqualifiedSName :: S.Symbol -> S.Name
unqualifiedSName = over S.nameConcrete NameUnqualified

checkUnqualified ::
  Members '[Error ScopeError, State Scope] r =>
  Symbol -> Sem r S.Name
checkUnqualified s = do
  -- Local vars have scope priority
  l <- HashMap.lookup s <$> gets _scopeLocalVars
  case l of
    Just LocalVariable {..} -> return (unqualifiedSName variableName)
    Nothing -> do
     -- Lookup at the global scope
     let err = throw (ErrSymNotInScope s)
     SymbolInfo {..} <- fromMaybeM err (HashMap.lookup s <$> gets _scopeSymbols)
     case HashMap.toList symbolInfo of
      [] -> error "impossible"
      [(_, e)] -> return (symbolToSName s e)
      es -> throw (ErrAmbiguousSym es)

symbolToSName :: Symbol -> SymbolEntry -> S.Name
symbolToSName s SymbolEntry {..} =  S.Name' {
        _nameId = symbolId,
        _nameConcrete = NameUnqualified s,
        _nameDefinedIn = symbolDefinedIn,
        _nameKind = symbolKind }

checkPatternName :: Members '[Error ScopeError, State Scope, State ScopeState] r =>
 Name -> Sem r S.Name
checkPatternName n = case n of
  NameQualified _ -> undefined
  NameUnqualified s -> checkPatternSymbol s

bindLocalVariable :: forall r. Members '[Error ScopeError, State Scope, State ScopeState] r =>
  Symbol -> Sem r S.Symbol
bindLocalVariable s = do
  checkNotInGroup
  addToGroup
  where
  checkNotInGroup :: Sem r ()
  checkNotInGroup = whenJustM (HashMap.lookup s <$> gets _scopeBindGroup)
   (const (throw (ErrBindGroup s)))
  addToGroup :: Sem r S.Symbol
  addToGroup = do
    n <- freshVariable s
    modify (over scopeBindGroup (HashMap.insert s (LocalVariable n)))
    return n


checkPatternSymbol :: forall r. Members '[Error ScopeError, State Scope, State ScopeState] r =>
 Symbol -> Sem r S.Name
checkPatternSymbol s = do
  c <- isConstructor
  case c of
    Just constr -> return constr -- the symbol is a constructor
    Nothing -> unqualifiedSName <$> bindLocalVariable s -- the symbol is a variable
  -- check whether the symbol is a constructor in scope
  where
  isConstructor :: Sem r (Maybe S.Name)
  isConstructor = do
    r <- HashMap.lookup s <$> gets _scopeSymbols
    case r of
      Nothing -> return Nothing
      Just SymbolInfo {..} ->
        let entries = filter (isConstructorKind . symbolKind . snd) (HashMap.toList symbolInfo)
        in case map snd entries of
          [] -> return Nothing -- There is no constructor with such a name
          [e] -> return (Just (symbolToSName s e)) -- There is one constructor with such a name
          _ -> throw Err -- There is more than one constructor with such a name
  isConstructorKind :: S.NameKind -> Bool
  isConstructorKind = (== S.KNameConstructor)


checkPatternSections :: Members '[Error ScopeError, State Scope, State ScopeState] r =>
 PatternSections 'Parsed -> Sem r (PatternSections 'Scoped)
checkPatternSections (PatternSections s) = PatternSections <$> mapM checkPatternSection s

checkPatternSection :: Members '[Error ScopeError, State Scope, State ScopeState] r =>
 PatternSection 'Parsed -> Sem r (PatternSection 'Scoped)
checkPatternSection p = case p of
  PatternSectionWildcard -> return PatternSectionWildcard
  PatternSectionEmpty -> return PatternSectionEmpty
  PatternSectionParen e -> PatternSectionParen <$> checkPatternSections e
  PatternSectionName n -> PatternSectionName <$> checkPatternName n

checkName ::
  Members '[Error ScopeError, State Scope] r =>
  Name -> Sem r S.Name
checkName n = case n of
  NameQualified q -> checkQualified q
  NameUnqualified s -> checkUnqualified s

checkExpressionSection ::
  Members '[Error ScopeError, State Scope] r =>
  ExpressionSection 'Parsed ->
  Sem r (ExpressionSection 'Scoped)
checkExpressionSection e = case e of
  SectionIdentifier n -> SectionIdentifier <$> checkName n
  SectionLambda lam -> SectionLambda <$> checkLambda lam
  SectionLetBlock letBlock -> SectionLetBlock <$> checkLetBlock letBlock
  SectionUniverse uni -> return (SectionUniverse uni)
  SectionFunction fun -> SectionFunction <$> checkFunction fun
  SectionParens par -> SectionParens <$> checkExpressionSections par
  SectionFunArrow -> return SectionFunArrow
  SectionMatch match -> SectionMatch <$> checkMatch match

checkMatch ::
  Members '[Error ScopeError, State Scope] r =>
  Match 'Parsed ->
  Sem r (Match 'Scoped)
checkMatch = undefined

checkExpressionSections ::
  Members '[Error ScopeError, State Scope] r =>
  ExpressionSections 'Parsed ->
  Sem r (ExpressionSections 'Scoped)
checkExpressionSections (ExpressionSections l) = ExpressionSections <$> mapM checkExpressionSection l

checkParseExpressionSections ::
  Members '[Error ScopeError, State Scope] r =>
  ExpressionSections 'Parsed ->
  Sem r Expression
checkParseExpressionSections = checkExpressionSections >=> parseExpressionSections

checkStatement ::
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO, State ModulesCache, State Scope] r =>
  Statement 'Parsed ->
  Sem r (Statement 'Scoped)
checkStatement s = case s of
  StatementOperator opDef -> StatementOperator opDef <$ addOperatorSyntaxDef opDef
  StatementTypeSignature tySig -> StatementTypeSignature <$> checkTypeSignature tySig
  StatementImport imp -> StatementImport <$> checkImport imp
  StatementDataType dt -> StatementDataType <$> checkDataTypeDef dt
  StatementModule dt -> StatementModule <$> checkLocalModule dt
  StatementOpenModule open -> StatementOpenModule open <$ checkOpenModule open
  StatementFunctionClause clause -> StatementFunctionClause <$> checkFunctionClause clause
  StatementAxiom ax -> StatementAxiom <$> checkAxiom ax
  StatementEval e -> StatementEval <$> checkEval e
  StatementPrint e -> StatementPrint <$> checkPrint e

-------------------------------------------------------------------------------
-- Infix Parsers
-------------------------------------------------------------------------------

parseExpressionSections ::
  Members '[Error ScopeError, State Scope] r =>
  ExpressionSections 'Scoped ->
  Sem r Expression
parseExpressionSections = undefined

parsePatternSections ::
  Members '[Error ScopeError, State Scope] r =>
  PatternSections 'Scoped ->
  Sem r Pattern
parsePatternSections = undefined

parsePatternTerm :: MonadParsec e [PatternSections 'Scoped] m => m Pattern
parsePatternTerm = undefined

parsePatternWildcard :: MonadParsec e [PatternSection 'Scoped] m => m Pattern
parsePatternWildcard = PatternWildcard <$ P.satisfy isWildcard
  where
    isWildcard PatternSectionWildcard = True
    isWildcard _ = False

tmp :: MonadParsec e [PatternSections 'Scoped] m => Scope -> m Pattern
tmp = undefined
