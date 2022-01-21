{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Limitations:
-- 1. A symbol introduced by a type signature can only be used once per Module.
module MiniJuvix.Syntax.Concrete.Scoped.Scoper where

import qualified Control.Monad.Combinators.Expr as P
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Stream as Stream
import qualified Data.Text as Text
import Lens.Micro.Platform
import qualified MiniJuvix.Syntax.Concrete.Base as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Parser (runModuleParserIO)
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameKind (KNameConstructor))
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Utils.Prelude
import System.FilePath

--------------------------------------------------------------------------------

-- | Relevant scope information of a module.
data ModuleScopeInfo = ModuleScopeInfo
  { -- | Absolute path to the module
    _syntaxPath :: S.AbsModulePath,
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

newtype SymbolInfo = SymbolInfo
  { -- | This map must have at least one entry.
    -- If there are more than one entry, it means that the same symbol has been
    -- brought into scope from two different places.
    symbolInfo :: HashMap S.AbsModulePath SymbolEntry
  }

data SymbolEntry = SymbolEntry
  { symbolKind :: S.NameKind,
    symbolDefinedIn :: S.AbsModulePath,
    symbolId :: S.NameId,
    symbolFixity :: S.NameFixity
  }
  deriving stock (Show)

data Scope = Scope
  { _scopePath :: S.AbsModulePath,
    _scopeFixities :: HashMap Symbol Fixity,
    _scopeSymbols :: HashMap Symbol SymbolInfo,
    _scopeModules :: HashMap QualifiedName ModuleScopeInfo,
    _scopeBindGroup :: HashMap Symbol LocalVariable
  }

makeLenses ''Scope

newtype LocalVars = LocalVars
  { _localVars :: HashMap Symbol LocalVariable
  }

makeLenses ''LocalVars

newtype ModulesCache = ModulesCache
  { _cachedModules :: HashMap TopModulePath (Module 'Scoped 'ModuleTop)
  }

makeLenses ''ModulesCache

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
  | ErrBindGroup Symbol
  | ErrDuplicateFixity Symbol
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
  { _scopeModulesCache :: ModulesCache,
    _scopeFreeNames :: Stream S.NameId
  }

makeLenses ''ScopeState

scopeCheck1 :: FilePath -> Module 'Parsed 'ModuleTop -> IO (Either ScopeError (Module 'Scoped 'ModuleTop))
scopeCheck1 root m = fmap head <$> scopeCheck root (pure m)

scopeCheck :: FilePath -> NonEmpty (Module 'Parsed 'ModuleTop) -> IO (Either ScopeError (NonEmpty (Module 'Scoped 'ModuleTop)))
scopeCheck root modules =
  runM $
    runError $
      runReader scopeParameters $
        evalState iniScopeState $
          mapM checkTopModule modules
  where
    iniScopeState :: ScopeState
    iniScopeState =
      ScopeState
        { _scopeModulesCache = ModulesCache mempty,
          _scopeFreeNames = S.allNameIds
        }
    scopeParameters :: ScopeParameters
    scopeParameters =
      ScopeParameters
        { _scopeRootPath = root,
          _scopeFileExtension = ".mjuvix",
          _scopeTopParents = mempty
        }

freshNameId ::
  Members '[State ScopeState] r =>
  Sem r S.NameId
freshNameId = do
  i <- gets (Stream.head . _scopeFreeNames)
  modify (over scopeFreeNames Stream.tail)
  return i

freshVariable :: Members '[State ScopeState, State Scope] r => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol S.KNameLocal

freshSymbol ::
  forall r.
  Members '[State ScopeState, State Scope] r =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
freshSymbol _nameKind _nameConcrete = do
  _nameId <- freshNameId
  _nameDefinedIn <- gets _scopePath
  _nameFixity <- getFixity
  return S.Name' {..}
  where
    getFixity :: Sem r S.NameFixity
    getFixity
      | S.canHaveFixity _nameKind = do
        maybe S.NoFixity S.SomeFixity . HashMap.lookup _nameConcrete <$> gets _scopeFixities
      | otherwise = return S.NoFixity

reserveSymbolOf ::
  forall r.
  Members '[Error ScopeError, State ScopeState, State Scope] r =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolOf k s = do
  checkNotBound
  freshSymbol k s
  where
    checkNotBound :: Sem r ()
    checkNotBound = do
      path <- gets _scopePath
      syms <- gets _scopeSymbols
      let exists = maybe False (HashMap.member path . symbolInfo) (HashMap.lookup s syms)
      when exists (throw (ErrAlreadyDefined s))

bindReservedSymbol ::
  Members '[State Scope] r =>
  S.Symbol ->
  Sem r ()
bindReservedSymbol s' = do
  path <- gets _scopePath
  modify (over scopeSymbols (HashMap.alter (Just . addS path) s))
  where
    s = S._nameConcrete s'
    entry :: SymbolEntry
    entry = symbolEntry s'
    addS :: S.AbsModulePath -> Maybe SymbolInfo -> SymbolInfo
    addS path m = case m of
      Nothing -> SymbolInfo (HashMap.singleton path entry)
      Just SymbolInfo {..} -> SymbolInfo (HashMap.insert path entry symbolInfo)

bindSymbolOf ::
  Members '[Error ScopeError, State ScopeState, State Scope] r =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
bindSymbolOf k s = do
  s' <- reserveSymbolOf k s
  bindReservedSymbol s'
  return s'

bindFunctionSymbol ::
  Members '[Error ScopeError, State ScopeState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindFunctionSymbol = bindSymbolOf S.KNameFunction

bindInductiveSymbol ::
  Members '[Error ScopeError, State ScopeState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindInductiveSymbol = bindSymbolOf S.KNameInductive

bindAxiomSymbol ::
  Members '[Error ScopeError, State ScopeState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindAxiomSymbol = bindSymbolOf S.KNameAxiom

bindLocalModuleSymbol ::
  Members '[Error ScopeError, State ScopeState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindLocalModuleSymbol = bindSymbolOf S.KNameLocalModule

bindConstructorSymbol ::
  Members '[Error ScopeError, State ScopeState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindConstructorSymbol = bindSymbolOf S.KNameConstructor

checkImport ::
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, Embed IO, State ScopeState] r =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImport (Import p) = Import <$> (readParseModule p >>= checkTopModule)

getTopModulePath :: Module s 'ModuleTop -> S.AbsModulePath
getTopModulePath Module {..} =
  S.AbsModulePath
    { S.absTopModulePath = modulePath,
      S.absLocalPath = mempty
    }

localModuleScopeInfo :: S.AbsModulePath -> Module 'Scoped 'ModuleLocal -> ModuleScopeInfo
localModuleScopeInfo parentPath lmod = moduleScopeInfo (parentPath S.<.> localModSym) lmod
  where
    localModSym :: Symbol
    localModSym = S._nameConcrete (modulePath lmod)

topModuleScopeInfo :: Module 'Scoped 'ModuleTop -> ModuleScopeInfo
topModuleScopeInfo m = moduleScopeInfo (getTopModulePath m) m

moduleScopeInfo :: S.AbsModulePath -> Module 'Scoped t -> ModuleScopeInfo
moduleScopeInfo absPath sModule = ModuleScopeInfo {..}
  where
    stmts = moduleBody sModule
    _syntaxPath :: S.AbsModulePath
    _syntaxPath = undefined
    _syntaxLocalModules :: HashMap Symbol ModuleScopeInfo
    _syntaxLocalModules = HashMap.fromList (mapMaybe getModule stmts)
      where
        getModule :: Statement 'Scoped -> Maybe (Symbol, ModuleScopeInfo)
        getModule s = case s of
          StatementModule m -> Just (S._nameConcrete (modulePath m), moduleScopeInfo undefined m)
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
    Left err -> throw (ErrParser err)
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

checkOperatorSyntaxDef ::
  forall r.
  Members '[Error ScopeError, State Scope] r =>
  OperatorSyntaxDef ->
  Sem r ()
checkOperatorSyntaxDef OperatorSyntaxDef {..} = do
  checkNotDefined
  modify (over scopeFixities (HashMap.insert opSymbol opFixity))
  where
    checkNotDefined :: Sem r ()
    checkNotDefined =
      whenM
        (HashMap.member opSymbol <$> gets _scopeFixities)
        (throw (ErrDuplicateFixity opSymbol))

checkTypeSignature ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature TypeSignature {..} = do
  sigType' <- localScope (checkParseExpressionAtoms sigType)
  sigName' <- bindFunctionSymbol sigName
  return
    TypeSignature
      { sigName = sigName',
        sigType = sigType'
      }

symbolEntry :: S.Symbol -> SymbolEntry
symbolEntry S.Name' {..} =
  SymbolEntry
    { symbolKind = _nameKind,
      symbolDefinedIn = _nameDefinedIn,
      symbolId = _nameId,
      symbolFixity = _nameFixity
    }

checkConstructorDef ::
  Members '[Error ScopeError, Reader LocalVars, State Scope, State ScopeState] r =>
  DataConstructorDef 'Parsed ->
  Sem r (DataConstructorDef 'Scoped)
checkConstructorDef DataConstructorDef {..} = do
  constructorType' <- checkParseExpressionAtoms constructorType
  constructorName' <- bindConstructorSymbol constructorName
  return
    DataConstructorDef
      { constructorName = constructorName',
        constructorType = constructorType'
      }

checkDataTypeDef ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  DataTypeDef 'Parsed ->
  Sem r (DataTypeDef 'Scoped)
checkDataTypeDef DataTypeDef {..} = do
  localScope $ checkDataTypeRec dataTypeParameters
  where
    checkDataTypeRec ::
      forall r.
      Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
      [DataTypeParameter 'Parsed] ->
      Sem r (DataTypeDef 'Scoped)
    checkDataTypeRec dtp = go dtp []
      where
        go :: [DataTypeParameter 'Parsed] -> [DataTypeParameter 'Scoped] -> Sem r (DataTypeDef 'Scoped)
        go params dataTypeParameters' =
          case params of
            -- More params to check
            (DataTypeParameter {..} : ps) -> do
              dataTypeParameterType' <- checkParseExpressionAtoms dataTypeParameterType
              dataTypeParameterName' <- freshVariable dataTypeParameterName
              let param' =
                    DataTypeParameter
                      { dataTypeParameterType = dataTypeParameterType',
                        dataTypeParameterName = dataTypeParameterName'
                      }
              withBindLocalVariable (LocalVariable dataTypeParameterName') $
                go ps (dataTypeParameters' ++ [param'])
            -- All params have been checked
            [] -> do
              dataTypeType' <- sequence (checkParseExpressionAtoms <$> dataTypeType)
              dataTypeName' <- bindInductiveSymbol dataTypeName
              dataTypeConstructors' <- mapM checkConstructorDef dataTypeConstructors
              return
                DataTypeDef
                  { dataTypeName = dataTypeName',
                    dataTypeParameters = dataTypeParameters',
                    dataTypeType = dataTypeType',
                    dataTypeConstructors = dataTypeConstructors'
                  }

checkTopModule ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO, State ScopeState] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule m@(Module path stmts) = do
  checkCycle
  cache <- gets (_cachedModules . _scopeModulesCache)
  maybe checkedModule return (cache ^. at path)
  where
    iniScope :: Scope
    iniScope =
      Scope
        { _scopePath = getTopModulePath m,
          _scopeFixities = mempty,
          _scopeSymbols = mempty,
          _scopeModules = mempty,
          _scopeBindGroup = mempty
        }
    addParent :: ScopeParameters -> ScopeParameters
    addParent = over scopeTopParents (HashSet.insert path)
    checkedModule :: Sem r (Module 'Scoped 'ModuleTop)
    checkedModule =
      evalState iniScope $
        Module path
          <$> local addParent (mapM checkStatement stmts)
    checkCycle :: Sem r ()
    checkCycle =
      whenM
        (HashSet.member path <$> asks _scopeTopParents)
        (throw (ErrImportCycle path))

checkLocalModule ::
  forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScopeState, Embed IO] r =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule Module {..} = do
  modulePath' <- bindLocalModuleSymbol modulePath
  moduleBody' <- mapM checkStatement moduleBody
  return
    Module
      { modulePath = modulePath',
        moduleBody = moduleBody'
      }

-- | checks if there is an infix declaration without a binding.
checkOrphanFixities :: Members '[Error ScopeError, State Scope] r => Sem r ()
checkOrphanFixities = undefined

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

openModule ::
  forall r.
  Members '[Error ScopeError, State Scope] r =>
  QualifiedName ->
  ModuleScopeInfo ->
  Maybe UsingHiding ->
  Sem r ()
openModule qname _ uh = do
  -- mapM_ (openOperatorSyntaxDef qname) (filter (shouldOpen . fst) (HashMap.toList _syntaxOperators))
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

checkWhereBlock ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  WhereBlock 'Parsed ->
  Sem r (WhereBlock 'Scoped)
checkWhereBlock WhereBlock {..} = WhereBlock <$> mapM checkWhereClause whereClauses

checkWhereClause ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  WhereClause 'Parsed ->
  Sem r (WhereClause 'Scoped)
checkWhereClause c = case c of
  WhereOpenModule _ -> undefined
  WhereTypeSig s -> WhereTypeSig <$> checkTypeSignature s
  WhereFunClause f -> WhereFunClause <$> checkFunctionClause f

checkFunctionClause ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  FunctionClause 'Parsed ->
  Sem r (FunctionClause 'Scoped)
checkFunctionClause FunctionClause {..} = do
  clauseOwnerFunction' <- checkSymbolInScope
  clausePatterns' <- mapM checkParsePatternAtom clausePatterns
  (clauseWhere', clauseBody') <- localScope $
    withBindCurrentGroup $ do
      clw <- sequence (checkWhereBlock <$> clauseWhere)
      clb <- checkParseExpressionAtoms clauseBody
      return (clw, clb)
  return
    FunctionClause
      { clauseOwnerFunction = clauseOwnerFunction',
        clausePatterns = clausePatterns',
        clauseBody = clauseBody',
        clauseWhere = clauseWhere'
      }
  where
    fun = clauseOwnerFunction
    checkSymbolInScope :: Sem r S.Symbol
    checkSymbolInScope = do
      SymbolInfo {..} <- fromMaybeM err (HashMap.lookup fun <$> gets _scopeSymbols)
      -- The symbol must be defined in the same path
      path <- gets _scopePath
      e@SymbolEntry {..} <- fromMaybe err (return <$> HashMap.lookup path symbolInfo)
      when (symbolKind /= S.KNameFunction) err
      return (entryToSName fun e)
      where
        err :: Sem r a
        err = throw (ErrLacksTypeSig fun)

checkAxiom ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiom AxiomDef {..} = do
  axiomName' <- bindAxiomSymbol axiomName
  axiomType' <- localScope $ checkParseExpressionAtoms axiomType
  return
    AxiomDef
      { axiomName = axiomName',
        axiomType = axiomType'
      }

localScope :: Sem (Reader LocalVars : r) a -> Sem r a
localScope = runReader (LocalVars mempty)

checkEval ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  Eval 'Parsed ->
  Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> localScope (checkParseExpressionAtoms s)

checkPrint ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  Print 'Parsed ->
  Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> localScope (checkParseExpressionAtoms s)

checkFunction ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction Function {..} = do
  funParameter' <- checkParam
  let scoped = case paramName funParameter' of
        Nothing -> id
        Just s -> withBindLocalVariable (LocalVariable s)
  funReturn' <- scoped (checkParseExpressionAtoms funReturn)
  return
    Function
      { funParameter = funParameter',
        funReturn = funReturn'
      }
  where
    checkParam :: Sem r (FunctionParameter 'Scoped)
    checkParam = do
      paramType' <- checkParseExpressionAtoms paramType
      paramName' <- checkParamName
      return
        FunctionParameter
          { paramName = paramName',
            paramUsage = paramUsage,
            paramType = paramType'
          }
      where
        FunctionParameter {..} = funParameter
        checkParamName :: Sem r (Maybe S.Symbol)
        checkParamName = case paramName of
          Nothing -> return Nothing
          Just s -> Just <$> freshVariable s

-- | Like a regular type signature?
checkLocalTypeSig ::
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkLocalTypeSig = checkTypeSignature

checkLetClause ::
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  LetClause 'Parsed ->
  Sem r (LetClause 'Scoped)
checkLetClause lc = case lc of
  LetTypeSig t -> LetTypeSig <$> checkLocalTypeSig t
  LetFunClause c -> LetFunClause <$> checkFunctionClause c

checkLetBlock ::
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  LetBlock 'Parsed ->
  Sem r (LetBlock 'Scoped)
checkLetBlock LetBlock {..} = do
  s <- get @Scope -- backup scope: we do not want local definitions to stay in scope
  letClauses' <- mapM checkLetClause letClauses
  letExpression' <- checkParseExpressionAtoms letExpression
  put s -- restore scope
  return
    LetBlock
      { letClauses = letClauses',
        letExpression = letExpression'
      }

checkLambda ::
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda Lambda {..} = Lambda <$> mapM checkLambdaClause lambdaClauses

checkLambdaClause ::
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  LambdaClause 'Parsed ->
  Sem r (LambdaClause 'Scoped)
checkLambdaClause LambdaClause {..} = do
  lambdaParameters' <- mapM checkParsePatternAtom lambdaParameters
  lambdaBody' <- withBindCurrentGroup (checkParseExpressionAtoms lambdaBody)
  return
    LambdaClause
      { lambdaParameters = lambdaParameters',
        lambdaBody = lambdaBody'
      }

checkQualified ::
  Members '[Error ScopeError, State Scope] r =>
  QualifiedName ->
  Sem r S.Name
checkQualified q = error "todo"

unqualifiedSName :: S.Symbol -> S.Name
unqualifiedSName = over S.nameConcrete NameUnqualified

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
      case HashMap.toList symbolInfo of
        [] -> error "impossible"
        [(_, e)] -> return (entryToSName (NameUnqualified s) e)
        es -> throw (ErrAmbiguousSym es) -- This is meant to happen only at the top level

entryToSName :: s -> SymbolEntry -> S.Name' s
entryToSName s SymbolEntry {..} =
  S.Name'
    { _nameId = symbolId,
      _nameConcrete = s,
      _nameDefinedIn = symbolDefinedIn,
      _nameFixity = symbolFixity,
      _nameKind = symbolKind
    }

checkPatternName ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  Name ->
  Sem r S.Name
checkPatternName n = case n of
  NameQualified _ -> error "todo"
  NameUnqualified s -> checkPatternUnqualified s

withBindCurrentGroup ::
  Members '[State Scope, Reader LocalVars] r =>
  Sem r a ->
  Sem r a
withBindCurrentGroup ma = do
  grp <- gets _scopeBindGroup
  modify (over scopeBindGroup (const mempty)) -- empties the group
  local (over localVars (HashMap.union grp)) ma

addLocalVars :: [LocalVariable] -> LocalVars -> LocalVars
addLocalVars lv = over localVars (flip (foldr insertVar) lv)
  where
    insertVar v = HashMap.insert (S._nameConcrete (variableName v)) v

withBindLocalVariable ::
  Members '[Reader LocalVars] r =>
  LocalVariable ->
  Sem r a ->
  Sem r a
withBindLocalVariable var = local (addLocalVars [var])

-- | Binds a local variable in a bind group, i.e. a pattern.
groupBindLocalVariable ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  Symbol ->
  Sem r S.Symbol
groupBindLocalVariable s = do
  checkNotInGroup
  addToGroup
  where
    checkNotInGroup :: Sem r ()
    checkNotInGroup =
      whenJustM
        (HashMap.lookup s <$> gets _scopeBindGroup)
        (const (throw (ErrBindGroup s)))
    addToGroup :: Sem r S.Symbol
    addToGroup = do
      n <- freshVariable s
      modify (over scopeBindGroup (HashMap.insert s (LocalVariable n)))
      return n

checkPatternUnqualified ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  Symbol ->
  Sem r S.Name
checkPatternUnqualified s = do
  c <- isConstructor
  case c of
    Just constr -> return constr -- the symbol is a constructor
    Nothing -> unqualifiedSName <$> groupBindLocalVariable s -- the symbol is a variable
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
                [e] -> return (Just (entryToSName (NameUnqualified s) e)) -- There is one constructor with such a name
                _ -> throw Err -- There is more than one constructor with such a name
    isConstructorKind :: S.NameKind -> Bool
    isConstructorKind = (== S.KNameConstructor)

checkPatternAtoms ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  PatternAtoms 'Parsed ->
  Sem r (PatternAtoms 'Scoped)
checkPatternAtoms (PatternAtoms s) = PatternAtoms <$> mapM checkPatternAtom s

checkPatternAtom ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  PatternAtom 'Parsed ->
  Sem r (PatternAtom 'Scoped)
checkPatternAtom p = case p of
  PatternAtomWildcard -> return PatternAtomWildcard
  PatternAtomEmpty -> return PatternAtomEmpty
  PatternAtomParens e -> PatternAtomParens <$> checkPatternAtoms e
  PatternAtomName n -> PatternAtomName <$> checkPatternName n

checkName ::
  Members '[Error ScopeError, State Scope, Reader LocalVars] r =>
  Name ->
  Sem r S.Name
checkName n = case n of
  NameQualified q -> checkQualified q
  NameUnqualified s -> checkUnqualified s

checkExpressionAtom ::
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  ExpressionAtom 'Parsed ->
  Sem r (ExpressionAtom 'Scoped)
checkExpressionAtom e = case e of
  AtomIdentifier n -> AtomIdentifier <$> checkName n
  AtomLambda lam -> AtomLambda <$> checkLambda lam
  AtomLetBlock letBlock -> AtomLetBlock <$> checkLetBlock letBlock
  AtomUniverse uni -> return (AtomUniverse uni)
  AtomFunction fun -> AtomFunction <$> checkFunction fun
  AtomParens par -> AtomParens <$> checkExpressionAtoms par
  AtomFunArrow -> return AtomFunArrow
  AtomMatch match -> AtomMatch <$> checkMatch match

checkMatchAlt ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScopeState] r =>
  MatchAlt 'Parsed ->
  Sem r (MatchAlt 'Scoped)
checkMatchAlt MatchAlt {..} = do
  matchAltPattern' <- checkParsePatternAtom matchAltPattern
  matchAltBody' <- withBindCurrentGroup (checkParseExpressionAtoms matchAltBody)
  return
    MatchAlt
      { matchAltPattern = matchAltPattern',
        matchAltBody = matchAltBody'
      }

checkMatch ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScopeState] r =>
  Match 'Parsed ->
  Sem r (Match 'Scoped)
checkMatch Match {..} = do
  matchExpression' <- checkParseExpressionAtoms matchExpression
  matchAlts' <- mapM checkMatchAlt matchAlts
  return
    Match
      { matchExpression = matchExpression',
        matchAlts = matchAlts'
      }

checkExpressionAtoms ::
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l) = ExpressionAtoms <$> mapM checkExpressionAtom l

checkParseExpressionAtoms ::
  Members '[Error ScopeError, State Scope, State ScopeState, Reader LocalVars] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  Members '[Error ScopeError, State Scope, State ScopeState] r =>
  PatternAtom 'Parsed ->
  Sem r Pattern
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkStatement ::
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO, State Scope, State ScopeState] r =>
  Statement 'Parsed ->
  Sem r (Statement 'Scoped)
checkStatement s = case s of
  StatementOperator opDef -> StatementOperator opDef <$ checkOperatorSyntaxDef opDef
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
-- Infix Expression
-------------------------------------------------------------------------------

makeExpressionTable ::
  forall r.
  (Members '[State Scope] r) =>
  Sem r [[P.Operator Parse Expression]]
makeExpressionTable = do
  symbolTable <- mkSymbolTable . toList <$> gets _scopeSymbols
  -- application has the highest precedence. Arrow has the lowest.
  return $ [appOp] : symbolTable ++ [[functionOp]]
  where
    -- TODO think what to do with qualified symbols
    mkSymbolTable :: [SymbolInfo] -> [[P.Operator Parse Expression]]
    mkSymbolTable = map (map snd) . groupSortOn fst . mapMaybe (unqualifiedSymbolOp . getEntry)
      where
        getEntry :: SymbolInfo -> SymbolEntry
        getEntry (SymbolInfo m) = case toList m of
          [] -> error "impossible"
          [e] -> e
          _ -> error "impossible after scope checking"
        unqualifiedSymbolOp :: SymbolEntry -> Maybe (Precedence, P.Operator Parse Expression)
        unqualifiedSymbolOp SymbolEntry {..}
          | S.SomeFixity Fixity {..} <- symbolFixity = Just $
            case fixityArity of
              Unary u -> (fixityPrecedence, prePost (unaryApp <$> parseSymbolId symbolId))
                where
                  unaryApp :: S.Name -> Expression -> Expression
                  unaryApp funName arg = case u of
                    -- TODO Prefix application
                    AssocPrefix -> ExpressionApplication (Application (ExpressionIdentifier funName) arg)
                    AssocPostfix -> ExpressionPostfixApplication (PostfixApplication funName arg)
                  prePost :: Parse (Expression -> Expression) -> P.Operator Parse Expression
                  prePost = case u of
                    AssocPrefix -> P.Prefix
                    AssocPostfix -> P.Postfix
              Binary b -> (fixityPrecedence, infixLRN (binaryApp <$> parseSymbolId symbolId))
                where
                  binaryApp :: S.Name -> Expression -> Expression -> Expression
                  binaryApp infixAppOperator infixAppLeft infixAppRight =
                    ExpressionInfixApplication InfixApplication {..}
                  infixLRN :: Parse (Expression -> Expression -> Expression) -> P.Operator Parse Expression
                  infixLRN = case b of
                    AssocLeft -> P.InfixL
                    AssocRight -> P.InfixR
                    AssocNone -> P.InfixN
          | otherwise = Nothing
        parseSymbolId :: S.NameId -> Parse S.Name
        parseSymbolId uid = P.token getName mempty
          where
            getName :: ExpressionAtom 'Scoped -> Maybe S.Name
            getName s = case s of
              AtomIdentifier n'
                | uid == S._nameId n' -> Just n'
              _ -> Nothing

    -- Application by juxtaposition.
    appOp :: P.Operator Parse Expression
    appOp = P.InfixL (app <$ notFollowedByInfix)
      where
        notFollowedByInfix :: Parse ()
        notFollowedByInfix = P.notFollowedBy (P.token infixName mempty)
          where
            infixName :: ExpressionAtom 'Scoped -> Maybe S.Name
            infixName s = case s of
              AtomIdentifier n
                | S.hasFixity n -> Just n
              _ -> Nothing

        app :: Expression -> Expression -> Expression
        app f x =
          ExpressionApplication
            Application
              { applicationFunction = f,
                applicationParameter = x
              }
    -- Non-dependent function type: A → B
    functionOp :: P.Operator Parse Expression
    functionOp = P.InfixR (nonDepFun <$ P.single AtomFunArrow)
      where
        nonDepFun :: Expression -> Expression -> Expression
        nonDepFun a b =
          ExpressionFunction
            Function
              { funParameter = param,
                funReturn = b
              }
          where
            param =
              FunctionParameter
                { paramName = Nothing,
                  paramUsage = Nothing,
                  paramType = a
                }

parseExpressionAtoms ::
  Members '[Error ScopeError, State Scope] r =>
  ExpressionAtoms 'Scoped ->
  Sem r Expression
parseExpressionAtoms (ExpressionAtoms sections) = do
  tbl <- makeExpressionTable
  let parser :: Parse Expression
      parser = runM (mkExpressionParser tbl) <* P.eof
      res = P.parse parser filePath (toList sections)
  case res of
    Left err -> throw (ErrInfixParser (show err))
    Right r -> return r
  where
    filePath = "tmp"

-- | Monad for parsing expression sections.
type Parse = P.Parsec () [ExpressionAtom 'Scoped]

-- data Parser tok m a where
--   EmbedParsec ::

mkExpressionParser ::
  [[P.Operator Parse Expression]] ->
  Sem '[Embed Parse] Expression
mkExpressionParser table = embed @Parse pExpression
  where
    pExpression :: Parse Expression
    pExpression = P.makeExprParser pTerm table
    pTerm :: Parse Expression
    pTerm = runM parseTermRec
      where
        parseTermRec :: Sem '[Embed Parse] Expression
        parseTermRec = runReader pExpression parseTerm

parseTerm :: forall r. Members '[Reader (Parse Expression), Embed Parse] r => Sem r Expression
parseTerm = do
  pExpr <- ask
  embed @Parse $
    parseUniverse
      <|> parseNoInfixIdentifier
      <|> parseParens pExpr
      <|> parseFunction
      <|> parseLambda
      <|> parseMatch
      <|> parseLetBlock
  where
    parseLambda :: Parse Expression
    parseLambda = ExpressionLambda <$> P.token lambda mempty
      where
        lambda :: ExpressionAtom 'Scoped -> Maybe (Lambda 'Scoped)
        lambda s = case s of
          AtomLambda l -> Just l
          _ -> Nothing

    parseMatch :: Parse Expression
    parseMatch = ExpressionMatch <$> P.token match mempty
      where
        match :: ExpressionAtom 'Scoped -> Maybe (Match 'Scoped)
        match s = case s of
          AtomMatch l -> Just l
          _ -> Nothing

    parseUniverse :: Parse Expression
    parseUniverse = ExpressionUniverse <$> P.token universe' mempty
      where
        universe' :: ExpressionAtom 'Scoped -> Maybe Universe
        universe' s = case s of
          AtomUniverse u -> Just u
          _ -> Nothing

    parseFunction :: Parse Expression
    parseFunction = ExpressionFunction <$> P.token function mempty
      where
        function :: ExpressionAtom 'Scoped -> Maybe (Function 'Scoped)
        function s = case s of
          AtomFunction u -> Just u
          _ -> Nothing

    parseLetBlock :: Parse Expression
    parseLetBlock = ExpressionLetBlock <$> P.token letBlock mempty
      where
        letBlock :: ExpressionAtom 'Scoped -> Maybe (LetBlock 'Scoped)
        letBlock s = case s of
          AtomLetBlock u -> Just u
          _ -> Nothing

    parseNoInfixIdentifier :: Parse Expression
    parseNoInfixIdentifier = ExpressionIdentifier <$> P.token identifierNoFixity mempty
      where
        identifierNoFixity :: ExpressionAtom 'Scoped -> Maybe S.Name
        identifierNoFixity s = case s of
          AtomIdentifier n
            | not (S.hasFixity n) -> Just n
          _ -> Nothing

    parseParens :: Parse Expression -> Parse Expression
    parseParens expressionParer = do
      exprs <- P.token parenExpr mempty
      case P.parse expressionParer strPath exprs of
        Right r -> return r
        Left {} -> P.failure Nothing mempty
      where
        strPath :: FilePath
        strPath = "inner parens"
        parenExpr :: ExpressionAtom 'Scoped -> Maybe [ExpressionAtom 'Scoped]
        parenExpr s = case s of
          AtomParens (ExpressionAtoms ss) -> Just (toList ss)
          _ -> Nothing

-------------------------------------------------------------------------------
-- Infix Patterns
-------------------------------------------------------------------------------

type ParsePat = P.Parsec () [PatternAtom 'Scoped]

makePatternTable ::
  forall r.
  (Members '[State Scope] r) =>
  Sem r [[P.Operator ParsePat Pattern]]
makePatternTable = do
  symbolTable <- mkSymbolTable . toList <$> gets _scopeSymbols
  -- application has the highest precedence.
  return $ [appOp] : symbolTable
  where
    -- TODO think what to do with qualified symbols
    mkSymbolTable :: [SymbolInfo] -> [[P.Operator ParsePat Pattern]]
    mkSymbolTable = map (map snd) . groupSortOn fst . mapMaybe (unqualifiedSymbolOp . getEntry)
      where
        nameToPattern :: S.Name -> Pattern
        nameToPattern n@S.Name' {..} = case _nameKind of
          S.KNameConstructor -> PatternConstructor n
          S.KNameLocal
            | NameUnqualified s <- _nameConcrete -> PatternVariable S.Name' {S._nameConcrete = s, ..}
          _ -> error "impossible"
        getEntry :: SymbolInfo -> SymbolEntry
        getEntry (SymbolInfo m) = case toList m of
          [] -> error "impossible"
          [e] -> e
          _ -> error "impossible after scope checking"
        unqualifiedSymbolOp :: SymbolEntry -> Maybe (Precedence, P.Operator ParsePat Pattern)
        unqualifiedSymbolOp SymbolEntry {..}
          | S.SomeFixity Fixity {..} <- symbolFixity,
            symbolKind == KNameConstructor = Just $
            case fixityArity of
              Unary u -> (fixityPrecedence, prePost (unaryApp <$> parseSymbolId symbolId))
                where
                  unaryApp :: S.Name -> Pattern -> Pattern
                  unaryApp funName = case u of
                    AssocPrefix -> PatternPrefixApplication . PatternPrefixApp funName
                    AssocPostfix -> PatternPostfixApplication . PatternPostfixApp funName
                  prePost :: ParsePat (Pattern -> Pattern) -> P.Operator ParsePat Pattern
                  prePost = case u of
                    AssocPrefix -> P.Prefix
                    AssocPostfix -> P.Postfix
              Binary b -> (fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId symbolId))
                where
                  binaryInfixApp :: S.Name -> Pattern -> Pattern -> Pattern
                  binaryInfixApp name argLeft = PatternInfixApplication . PatternInfixApp name argLeft
                  infixLRN :: ParsePat (Pattern -> Pattern -> Pattern) -> P.Operator ParsePat Pattern
                  infixLRN = case b of
                    AssocLeft -> P.InfixL
                    AssocRight -> P.InfixR
                    AssocNone -> P.InfixN
          | otherwise = Nothing
        parseSymbolId :: S.NameId -> ParsePat S.Name
        parseSymbolId uid = P.token getName mempty
          where
            getName :: PatternAtom 'Scoped -> Maybe S.Name
            getName s = case s of
              PatternAtomName n'
                | uid == S._nameId n' -> Just n'
              _ -> Nothing

    -- Application by juxtaposition.
    appOp :: P.Operator ParsePat Pattern
    appOp = P.InfixL (PatternApplication <$ notFollowedByInfix)
      where
        notFollowedByInfix :: ParsePat ()
        notFollowedByInfix = P.notFollowedBy (P.token infixName mempty)
          where
            infixName :: PatternAtom 'Scoped -> Maybe S.Name
            infixName s = case s of
              PatternAtomName n
                | S.hasFixity n -> Just n
              _ -> Nothing

parsePrePatTerm ::
  forall r.
  Members '[Reader (ParsePat Pattern), Embed ParsePat] r =>
  Sem r Pattern
parsePrePatTerm = do
  pPat <- ask
  embed @ParsePat $
    parseNoInfixConstructor
      <|> parseVariable
      <|> parseParens pPat
      <|> parseWildcard
      <|> parseEmpty
  where
    parseNoInfixConstructor :: ParsePat Pattern
    parseNoInfixConstructor =
      PatternConstructor
        <$> P.token constructorNoFixity mempty
      where
        constructorNoFixity :: PatternAtom 'Scoped -> Maybe S.Name
        constructorNoFixity s = case s of
          PatternAtomName n
            | not (S.hasFixity n) -> Just n
          _ -> Nothing

    parseWildcard :: ParsePat Pattern
    parseWildcard = PatternWildcard <$ P.satisfy isWildcard
      where
        isWildcard :: PatternAtom 'Scoped -> Bool
        isWildcard s = case s of
          PatternAtomWildcard -> True
          _ -> False

    parseEmpty :: ParsePat Pattern
    parseEmpty = PatternWildcard <$ P.satisfy isEmpty
      where
        isEmpty :: PatternAtom 'Scoped -> Bool
        isEmpty s = case s of
          PatternAtomEmpty -> True
          _ -> False

    parseVariable :: ParsePat Pattern
    parseVariable = PatternWildcard <$ P.token var mempty
      where
        var :: PatternAtom 'Scoped -> Maybe S.Symbol
        var s = case s of
          PatternAtomName S.Name' {..}
            | NameUnqualified sym <- _nameConcrete,
              S.KNameLocal <- _nameKind ->
              Just
                S.Name'
                  { S._nameConcrete = sym,
                    ..
                  }
          _ -> Nothing

    parseParens :: ParsePat Pattern -> ParsePat Pattern
    parseParens patternParser = do
      exprs <- P.token parenPat mempty
      case P.parse patternParser strPath exprs of
        Right r -> return r
        Left {} -> mzero
      where
        strPath :: FilePath
        strPath = "inner parens"
        parenPat :: PatternAtom 'Scoped -> Maybe [PatternAtom 'Scoped]
        parenPat s = case s of
          PatternAtomParens (PatternAtoms ss) -> Just (toList ss)
          _ -> Nothing

mkPatternParser ::
  forall r.
  Members '[Embed ParsePat] r =>
  [[P.Operator ParsePat Pattern]] ->
  Sem r Pattern
mkPatternParser table = embed @ParsePat pPattern
  where
    pPattern :: ParsePat Pattern
    pPattern = P.makeExprParser pTerm table
    pTerm :: ParsePat Pattern
    pTerm = runM parseTermRec
      where
        parseTermRec :: Sem '[Embed ParsePat] Pattern
        parseTermRec = runReader pPattern parsePrePatTerm

parsePatternAtom ::
  Members '[Error ScopeError, State Scope] r => PatternAtom 'Scoped -> Sem r Pattern
parsePatternAtom sec = do
  tbl <- makePatternTable
  let parser :: ParsePat Pattern
      parser = runM (mkPatternParser tbl) <* P.eof
      res = P.parse parser filePath [sec]
  case res of
    Left err -> throw (ErrInfixPattern (show err))
    Right r -> return r
  where
    filePath = "tmp"
