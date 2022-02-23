-- | Limitations:
-- 1. A symbol introduced by a type signature can only be used once per Module.
--
-- Efficiency considerations:
-- 1. The expression parser should be cached somehow. Consider Polysemy.View
module MiniJuvix.Syntax.Concrete.Scoped.Scoper where

import qualified Control.Monad.Combinators.Expr as P
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Stream as Stream
import Lens.Micro.Platform
import qualified MiniJuvix.Syntax.Concrete.Base as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Parser (runModuleParser)
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import MiniJuvix.Syntax.Concrete.Scoped.Error
import MiniJuvix.Prelude
import qualified Data.List.NonEmpty as NonEmpty
import MiniJuvix.Syntax.Concrete.Scoped.Scoper.Files

--------------------------------------------------------------------------------

scopeCheck1IO :: FilePath -> Module 'Parsed 'ModuleTop -> IO (Either ScopeError (Module 'Scoped 'ModuleTop))
scopeCheck1IO root = runM . runFilesIO . scopeCheck1 root

scopeCheck1Pure :: HashMap FilePath Text -> FilePath -> Module 'Parsed 'ModuleTop -> IO (Either ScopeError (Module 'Scoped 'ModuleTop))
scopeCheck1Pure fs root = runM . runFilesPure fs . scopeCheck1 root

scopeCheck1 :: Member Files r =>
  FilePath -> Module 'Parsed 'ModuleTop -> Sem r (Either ScopeError (Module 'Scoped 'ModuleTop))
scopeCheck1 root m = fmap head <$> scopeCheck root (pure m)

scopeCheck :: Member Files r =>
    FilePath -> NonEmpty (Module 'Parsed 'ModuleTop) -> Sem r (Either ScopeError (NonEmpty (Module 'Scoped 'ModuleTop)))
scopeCheck root modules =
    runError $
      runReader scopeParameters $
        evalState iniScoperState $
          mapM checkTopModule_ modules
  where
    iniScoperState :: ScoperState
    iniScoperState =
      ScoperState
        { _scoperModulesCache = ModulesCache mempty,
          _scoperFreeNames = S.allNameIds,
          _scoperModules = mempty
        }
    scopeParameters :: ScopeParameters
    scopeParameters =
      ScopeParameters
        { _scopeRootPath = root,
          _scopeFileExtension = ".mjuvix",
          _scopeTopParents = mempty
        }

freshNameId ::
  Members '[State ScoperState] r =>
  Sem r S.NameId
freshNameId = do
  i <- gets (Stream.head . _scoperFreeNames)
  modify (over scoperFreeNames Stream.tail)
  return i

freshVariable :: Members '[State ScoperState, State Scope] r => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol S.KNameLocal

freshSymbol ::
  forall r.
  Members '[State ScoperState, State Scope] r =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
freshSymbol _nameKind _nameConcrete = do
  _nameId <- freshNameId
  _nameDefinedIn <- gets _scopePath
  let _nameDefined = getLoc _nameConcrete
      _nameWhyInScope = S.BecauseDefined
      _namePublicAnn = NoPublic
  _nameFixity <- fixity
  return S.Name' {..}
  where
    fixity :: Sem r (Maybe Fixity)
    fixity
      | S.canHaveFixity _nameKind =
        fmap opFixity . HashMap.lookup _nameConcrete <$> gets _scopeFixities
      | otherwise = return Nothing

reserveSymbolOf ::
  forall r.
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
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
    let exists = HashMap.lookup s syms >>= HashMap.lookup path . _symbolInfo
    whenJust exists $
      \ e -> throw (ErrMultipleDeclarations
         MultipleDeclarations  {
         _multipleDeclEntry = e,
         _multipleDeclSymbol = _symbolText s,
         _multipleDeclSecond = getLoc s
         } )

symbolEntry :: S.Name' a -> SymbolEntry
symbolEntry S.Name' {..} = S.Name'
  { _nameConcrete = (), ..}

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
      Nothing -> symbolInfoSingle entry
      Just SymbolInfo {..} -> SymbolInfo (HashMap.insert path entry _symbolInfo)

bindSymbolOf ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
bindSymbolOf k s = do
  s' <- reserveSymbolOf k s
  bindReservedSymbol s'
  return s'

bindFunctionSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindFunctionSymbol = bindSymbolOf S.KNameFunction

bindInductiveSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindInductiveSymbol = bindSymbolOf S.KNameInductive

bindAxiomSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindAxiomSymbol = bindSymbolOf S.KNameAxiom

bindLocalModuleSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindLocalModuleSymbol = bindSymbolOf S.KNameLocalModule

bindConstructorSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  Symbol ->
  Sem r S.Symbol
bindConstructorSymbol = bindSymbolOf S.KNameConstructor

checkImport ::
  forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, Files, State ScoperState] r =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImport import_@(Import path) = do
  checkCycle
  cache <- gets (_cachedModules . _scoperModulesCache)
  entry' <- maybe (readParseModule path >>= local addImport . checkTopModule) return (cache ^. at path)
  let checked = _moduleEntryScoped entry'
      sname = checked ^. modulePath
      moduleId = S._nameId sname
  modify (over scopeTopModules (HashMap.insert path moduleId))
  let entry = mkModuleEntry entry'
  modify (over scoperModules (HashMap.insert moduleId entry))
  return (Import checked)
  where
  addImport :: ScopeParameters -> ScopeParameters
  addImport = over scopeTopParents (cons import_)
  checkCycle :: Sem r ()
  checkCycle = do
    topp <- asks _scopeTopParents
    case span (/= import_) topp of
      (_, []) -> return ()
      (c, _) -> let cyc = NonEmpty.reverse (import_ :| c)
                in throw (ErrImportCycle (ImportCycle cyc))


getTopModulePath :: Module 'Parsed 'ModuleTop -> S.AbsModulePath
getTopModulePath Module {..} =
  S.AbsModulePath
    { S.absTopModulePath = _modulePath,
      S.absLocalPath = mempty
    }

-- | Do not call directly. Looks for a symbol in (possibly) nested local modules
lookupSymbolAux :: forall r. (Members '[State Scope, Error ScopeError, State ScoperState] r) =>
   [Symbol] -> Symbol -> Sem r [SymbolEntry]
lookupSymbolAux modules final = do
  local' <- hereOrInLocalModule
  import' <- importedTopModule
  return $ local' ++ maybeToList import'
  where
  hereOrInLocalModule :: Sem r [SymbolEntry] =
    case modules of
      [] -> do
         r <- HashMap.lookup final <$> gets _scopeSymbols
         case r of
          Nothing -> return []
          Just SymbolInfo {..} -> case toList _symbolInfo of
            [] -> return []
            [e] -> return [e]
            es -> throw (ErrAmbiguousSym (AmbiguousSym es))
      (p : ps) -> do
        r <- fmap (filter (S.isModuleKind . S._nameKind) . toList . _symbolInfo)
          . HashMap.lookup p <$> gets _scopeSymbols
        case r of
          Just [x] -> do
            export <- getExportInfo (S._nameId x)
            maybeToList <$> lookInExport final ps export
          Just [] -> return []
          Just es -> throw $ ErrAmbiguousSym (AmbiguousSym es)
          Nothing -> return []
  importedTopModule :: Sem r (Maybe SymbolEntry)
  importedTopModule = do
    fmap mkEntry . HashMap.lookup path <$> gets _scopeTopModules
    where
    mkEntry modId = S.Name' {
         _nameConcrete = (),
         _nameKind = S.KNameTopModule,
         _nameDefinedIn = S.topModulePathToAbsPath path,
         _nameDefined = getLoc final,
         _nameId = modId,
         _nameFixity = Nothing,
         _nameWhyInScope = S.BecauseImportedOpened,
         _namePublicAnn = NoPublic}
    path = TopModulePath modules final

lookInExport :: forall r. Members '[State Scope, State ScoperState] r =>
  Symbol -> [Symbol] -> ExportInfo -> Sem r (Maybe SymbolEntry)
lookInExport sym remaining e = case remaining of
  [] -> return (HashMap.lookup sym (_exportSymbols e))
  (s : ss) -> do
    mexport <- mayModule e s
    case mexport of
      Nothing -> return Nothing
      Just e' -> lookInExport sym ss e'
 where
  mayModule :: ExportInfo -> Symbol -> Sem r (Maybe ExportInfo)
  mayModule ExportInfo {..} s =
    case do
      entry <- HashMap.lookup s _exportSymbols
      guard (S.isModuleKind (S._nameKind entry))
      return entry of
        Just entry -> Just <$> getExportInfo (S._nameId entry)
        Nothing -> return Nothing

-- | We return a list of entries because qualified names can point to different
-- modules due to nesting.
lookupQualifiedSymbol :: forall r. Members '[State Scope, Error ScopeError, State ScoperState] r =>
   ([Symbol], Symbol) -> Sem r [SymbolEntry]
lookupQualifiedSymbol (path, sym) = do
  here' <- here
  there' <- there
  return (here' ++ there')
  where
  -- | Current module.
  here :: Sem r [SymbolEntry]
  here = lookupSymbolAux path sym
  -- | Looks for a top level modules
  there :: Sem r [SymbolEntry]
  there = concatMapM (fmap maybeToList . uncurry lookInTopModule) allTopPaths
    where
    allTopPaths :: [(TopModulePath, [Symbol])]
    allTopPaths = map (first nonEmptyToTopPath) raw
      where
      lpath = toList path
      raw :: [(NonEmpty Symbol, [Symbol])]
      raw = [  (l, r) | i <- [1..length path],
              (Just l, r) <- [ first nonEmpty (splitAt i lpath) ]]
      nonEmptyToTopPath :: NonEmpty Symbol -> TopModulePath
      nonEmptyToTopPath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)
    lookInTopModule :: TopModulePath -> [Symbol] -> Sem r (Maybe SymbolEntry)
    lookInTopModule topPath remaining = do
      r <- HashMap.lookup topPath <$> gets _scopeTopModules
      case r of
        Nothing -> return Nothing
        Just i -> getExportInfo i >>= lookInExport sym remaining

checkQualifiedExpr ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  QualifiedName ->
  Sem r S.Name
checkQualifiedExpr q@(QualifiedName (Path p) sym) = do
   es <- lookupQualifiedSymbol (toList p, sym)
   case es of
     [] -> notInScope
     [e] -> return (entryToSName q' e)
     _ -> throw (ErrAmbiguousSym (AmbiguousSym es))
  where
  q' = NameQualified q
  notInScope = throw (ErrQualSymNotInScope q)

unqualifiedSName :: S.Symbol -> S.Name
unqualifiedSName = over S.nameConcrete NameUnqualified

entryToSName :: s -> SymbolEntry -> S.Name' s
entryToSName s S.Name' {..} = S.Name' {_nameConcrete = s, ..}

-- | We gather all symbols which have been defined or marked to be public in the given scope.
exportScope :: forall r. Members '[State Scope, Error ScopeError] r => Scope -> Sem r ExportInfo
exportScope Scope {..} = do
  _exportSymbols <- getExportSymbols
  return ExportInfo {..}
  where
    getExportSymbols :: Sem r (HashMap Symbol SymbolEntry)
    getExportSymbols = HashMap.fromList <$> mapMaybeM entry (HashMap.toList _scopeSymbols)
      where
        shouldExport :: SymbolEntry -> Bool
        shouldExport S.Name' {..} =
          _nameDefinedIn == _scopePath
            || _namePublicAnn == Public
        entry :: (Symbol, SymbolInfo) -> Sem r (Maybe (Symbol, SymbolEntry))
        entry (s, SymbolInfo {..}) =
          case filter shouldExport (toList _symbolInfo) of
            [] -> return Nothing
            [e] -> return $ Just (s, e)
            (e:es) -> throw (ErrMultipleExport
                       (MultipleExportConflict _scopePath  s (e :| es)))

readParseModule ::
  Members '[Error ScopeError, Reader ScopeParameters, Files] r =>
  TopModulePath ->
  Sem r (Module 'Parsed 'ModuleTop)
readParseModule mp = do
  path <- modulePathToFilePath mp
  txt <- readFile' path
  case runModuleParser path txt of
    Left err -> throw (ErrParser (MegaParsecError err))
    Right r -> return r

modulePathToFilePath ::
  Members '[Reader ScopeParameters] r =>
  TopModulePath ->
  Sem r FilePath
modulePathToFilePath mp = do
  root <- asks _scopeRootPath
  ext <- asks _scopeFileExtension
  return $ topModulePathToFilePath' (Just ext) root mp

checkOperatorSyntaxDef ::
  forall r.
  Members '[Error ScopeError, State Scope] r =>
  OperatorSyntaxDef ->
  Sem r ()
checkOperatorSyntaxDef s@OperatorSyntaxDef {..} = do
  checkNotDefined
  modify (over scopeFixities (HashMap.insert opSymbol s))
  where
    checkNotDefined :: Sem r ()
    checkNotDefined =
      whenJustM
        (HashMap.lookup opSymbol <$> gets _scopeFixities)
        (\s' -> throw (ErrDuplicateFixity (DuplicateFixity s' s)))

checkTypeSignature ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature TypeSignature {..} = do
  sigType' <- checkParseExpressionAtoms _sigType
  sigName' <- bindFunctionSymbol _sigName
  return
    TypeSignature
      { _sigName = sigName',
        _sigType = sigType'
      }

checkConstructorDef ::
  Members '[Error ScopeError, Reader LocalVars, State Scope, State ScoperState] r =>
  InductiveConstructorDef 'Parsed ->
  Sem r (InductiveConstructorDef 'Scoped)
checkConstructorDef InductiveConstructorDef {..} = do
  constructorType' <- checkParseExpressionAtoms constructorType
  constructorName' <- bindConstructorSymbol constructorName
  return
    InductiveConstructorDef
      { constructorName = constructorName',
        constructorType = constructorType'
      }

withParams :: forall r a. Members '[Reader LocalVars, Error ScopeError, State Scope, State ScoperState] r
  => [InductiveParameter 'Parsed] -> ([InductiveParameter 'Scoped] -> Sem r a) -> Sem r a
withParams xs a = go [] xs
  where
  go :: [InductiveParameter 'Scoped] -> [InductiveParameter 'Parsed] -> Sem r a
  go inductiveParameters' params =
    case params of
      -- More params to check
      (InductiveParameter {..} : ps) -> do
        inductiveParameterType' <- checkParseExpressionAtoms _inductiveParameterType
        inductiveParameterName' <- freshVariable _inductiveParameterName
        let param' =
              InductiveParameter
                { _inductiveParameterType = inductiveParameterType',
                  _inductiveParameterName = inductiveParameterName'
                }
        withBindLocalVariable (LocalVariable inductiveParameterName') $
          go (inductiveParameters' ++ [param']) ps
      -- All params have been checked
      [] -> a inductiveParameters'

checkInductiveDef :: forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  InductiveDef 'Parsed ->
  Sem r (InductiveDef 'Scoped)
checkInductiveDef InductiveDef {..} = do
  withParams _inductiveParameters $ \inductiveParameters' -> do
    inductiveType' <- sequence (checkParseExpressionAtoms <$> _inductiveType)
    inductiveName' <- bindInductiveSymbol _inductiveName
    inductiveConstructors' <- mapM checkConstructorDef _inductiveConstructors
    return InductiveDef
      { _inductiveName = inductiveName',
        _inductiveParameters = inductiveParameters',
        _inductiveType = inductiveType',
        _inductiveConstructors = inductiveConstructors'
      }

checkTopModule_ ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Files, State ScoperState] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule_ = fmap _moduleEntryScoped . checkTopModule

checkTopModule ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Files, State ScoperState] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (ModuleEntry' 'ModuleTop)
checkTopModule m@(Module path params body) = do
  r <- checkedModule
  modify (over (scoperModulesCache .  cachedModules) (HashMap.insert path r))
  return r
  where
  freshTopModulePath :: forall s. Members '[State ScoperState] s =>
     Sem s S.TopModulePath
  freshTopModulePath = do
    _nameId <- freshNameId
    let _nameDefinedIn = S.topModulePathToAbsPath path
        _nameConcrete = path
        _nameDefined = getLoc $ modulePathName path
        _nameKind = S.KNameTopModule
        _nameFixity = Nothing
        _namePublicAnn = NoPublic
        _nameWhyInScope = S.BecauseDefined
    return S.Name' {..}
  iniScope :: Scope
  iniScope = emptyScope (getTopModulePath m)
  checkedModule :: Sem r (ModuleEntry' 'ModuleTop)
  checkedModule = do
    evalState iniScope $ do
      path' <- freshTopModulePath
      localScope $ withParams params $ \params' -> do
        (_moduleEntryExport, body') <- checkModuleBody body
        let _moduleEntryScoped = Module {
          _modulePath = path',
          _moduleParameters = params',
          _moduleBody = body' }
        return ModuleEntry' {..}

withScope :: Members '[State Scope] r => Sem r a -> Sem r a
withScope ma = do
  before <- get @Scope
  x <- ma
  put before
  return x

checkModuleBody :: forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScoperState, Files, Reader LocalVars] r =>
  [Statement 'Parsed] ->
  Sem r (ExportInfo, [Statement 'Scoped])
checkModuleBody body = do
  body' <- mapM checkStatement body
  checkOrphanFixities
  checkClausesExist body'
  exported <- get >>= exportScope
  return (exported, body')

checkLocalModule ::
  forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScoperState, Files, Reader LocalVars] r =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule Module {..} = do
  (_moduleEntryExport, moduleBody', moduleParameters') <-
      withScope $ withParams _moduleParameters $ \p' -> do
    inheritScope
    (e, b) <- checkModuleBody _moduleBody
    return (e, b, p')
  modulePath' <- bindLocalModuleSymbol _modulePath
  let moduleId = S._nameId modulePath'
      _moduleEntryScoped = Module
        { _modulePath = modulePath',
          _moduleParameters = moduleParameters',
          _moduleBody = moduleBody'
        }
      entry = mkModuleEntry ModuleEntry' {..}
  modify (over scoperModules (HashMap.insert moduleId entry))
  return _moduleEntryScoped
  where
  inheritScope :: Sem r ()
  inheritScope = do
    absPath <- (S.<.> _modulePath) <$> gets _scopePath
    modify (set scopePath absPath)
    modify (over scopeSymbols (fmap inheritSymbol))
    modify (set scopeFixities mempty) -- do not inherit fixity declarations
    where
    inheritSymbol :: SymbolInfo -> SymbolInfo
    inheritSymbol (SymbolInfo s) = SymbolInfo (fmap inheritEntry s)
    inheritEntry :: SymbolEntry -> SymbolEntry
    inheritEntry = over S.nameWhyInScope S.BecauseInherited

checkClausesExist :: forall r. Members '[Error ScopeError, State Scope] r => [Statement 'Scoped] -> Sem r ()
checkClausesExist ss = whenJust msig (throw . ErrLacksFunctionClause . LacksFunctionClause)
  where
  msig = listToMaybe [ ts | StatementTypeSignature ts <- ss,
          null [ c | StatementFunctionClause c <- ss ,
                 c ^. clauseOwnerFunction == ts ^. sigName]]

checkOrphanFixities :: forall r. Members '[Error ScopeError, State Scope] r => Sem r ()
checkOrphanFixities = do
  path <- gets _scopePath
  declared <- gets _scopeFixities
  used <- gets (HashMap.keys . fmap (filter (== path) . HashMap.keys . _symbolInfo) . _scopeSymbols)
  let unused = toList $ foldr HashMap.delete declared used
  case unused of
    [] -> return ()
    (x : _) -> throw (ErrUnusedOperatorDef (UnusedOperatorDef x))

symbolInfoSingle :: SymbolEntry -> SymbolInfo
symbolInfoSingle p = SymbolInfo $ HashMap.singleton (S._nameDefinedIn p) p

lookupModuleSymbol :: Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Name -> Sem r S.Name
lookupModuleSymbol n = do
  es <- lookupQualifiedSymbol (path, sym)
  case filter (S.isModuleKind . S._nameKind) es of
    [] -> notInScope
    [x] -> return $ entryToSName n x
    _ -> throw (ErrAmbiguousModuleSym (AmbiguousModuleSym es))
  where
  notInScope = throw (ErrModuleNotInScope (ModuleNotInScope n))
  (path, sym) = case n of
    NameUnqualified s -> ([], s)
    NameQualified (QualifiedName (Path p) s) -> (toList p, s)

getExportInfo :: forall r. Members '[State ScoperState] r =>
 S.ModuleNameId -> Sem r ExportInfo
getExportInfo modId = do
  l <- HashMap.lookupDefault impossible modId
        <$> gets _scoperModules
  case l of
    _ :&: ent -> return $ _moduleEntryExport ent

checkOpenModule ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenModule OpenModule {..} = do
  openParameters' <- mapM checkParseExpressionAtoms openParameters
  openModuleName' <- lookupModuleSymbol openModuleName
  exported <- getExportInfo (S._nameId openModuleName')
  mergeScope (alterScope exported)
  return OpenModule {
    openModuleName = openModuleName',
    openParameters = openParameters',
    openUsingHiding = openUsingHiding,
    openPublic = openPublic
    }
  where
    mergeScope :: ExportInfo -> Sem r ()
    mergeScope ExportInfo {..} =
      mapM_ mergeSymbol (HashMap.toList _exportSymbols)
      where
        mergeSymbol :: (Symbol, SymbolEntry) -> Sem r ()
        mergeSymbol (s, entry) =
          modify
            (over scopeSymbols (HashMap.insertWith (<>) s (symbolInfoSingle entry)))
    setsUsingHiding :: Maybe (Either (HashSet Symbol) (HashSet Symbol))
    setsUsingHiding = case openUsingHiding of
      Just (Using l) -> Just (Left (HashSet.fromList (toList l)))
      Just (Hiding l) -> Just (Right (HashSet.fromList (toList l)))
      Nothing -> Nothing
    alterScope :: ExportInfo -> ExportInfo
    alterScope = alterEntries . filterScope
      where
        alterEntry :: SymbolEntry -> SymbolEntry
        alterEntry = set S.nameWhyInScope S.BecauseImportedOpened . set S.namePublicAnn openPublic
        alterEntries :: ExportInfo -> ExportInfo
        alterEntries = over exportSymbols (fmap alterEntry)
        filterScope :: ExportInfo -> ExportInfo
        filterScope = over exportSymbols filterTable
          where
            filterTable :: HashMap Symbol a -> HashMap Symbol a
            filterTable = HashMap.filterWithKey (const . shouldOpen)
        shouldOpen :: Symbol -> Bool
        shouldOpen s = case setsUsingHiding of
          Nothing -> True
          Just (Left using) -> HashSet.member s using
          Just (Right hiding) -> not (HashSet.member s hiding)

checkWhereBlock ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  WhereBlock 'Parsed ->
  Sem r (WhereBlock 'Scoped)
checkWhereBlock WhereBlock {..} = WhereBlock <$> mapM checkWhereClause whereClauses

checkWhereClause ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  WhereClause 'Parsed ->
  Sem r (WhereClause 'Scoped)
checkWhereClause c = case c of
  WhereOpenModule o -> WhereOpenModule <$> checkOpenModule o
  WhereTypeSig s -> WhereTypeSig <$> checkTypeSignature s
  WhereFunClause f -> WhereFunClause <$> checkFunctionClause f

checkFunctionClause ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  FunctionClause 'Parsed ->
  Sem r (FunctionClause 'Scoped)
checkFunctionClause clause@FunctionClause {..} = do
  clauseOwnerFunction' <- checkSymbolInScope
  (clausePatterns', clauseWhere', clauseBody') <- do
    clp <- mapM checkParsePatternAtom _clausePatterns
    withBindCurrentGroup $ do
      s <- get @Scope
      clw <- sequence (checkWhereBlock <$> _clauseWhere)
      clb <- checkParseExpressionAtoms _clauseBody
      put s
      return (clp, clw, clb)
  return
    FunctionClause
      { _clauseOwnerFunction = clauseOwnerFunction',
        _clausePatterns = clausePatterns',
        _clauseBody = clauseBody',
        _clauseWhere = clauseWhere'
      }
  where
    fun = _clauseOwnerFunction
    checkSymbolInScope :: Sem r S.Symbol
    checkSymbolInScope = do
      SymbolInfo {..} <- fromMaybeM err (HashMap.lookup fun <$> gets _scopeSymbols)
      -- The symbol must be defined in the same path
      path <- gets _scopePath
      e <- maybe err return (HashMap.lookup path _symbolInfo)
      when (S._nameKind e /= S.KNameFunction) err
      return (entryToSName fun e)
      where
        err :: Sem r a
        err = throw (ErrLacksTypeSig (LacksTypeSig clause))

checkAxiom ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiom AxiomDef {..} = do
  axiomName' <- bindAxiomSymbol _axiomName
  axiomType' <- localScope $ checkParseExpressionAtoms _axiomType
  return
    AxiomDef
      { _axiomName = axiomName',
        _axiomType = axiomType'
      }

localScope :: Sem (Reader LocalVars : r) a -> Sem r a
localScope = runReader (LocalVars mempty)

checkEval ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Eval 'Parsed ->
  Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> localScope (checkParseExpressionAtoms s)

checkPrint ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Print 'Parsed ->
  Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> localScope (checkParseExpressionAtoms s)

checkFunction ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
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
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkLocalTypeSig = checkTypeSignature

checkLetClause ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  LetClause 'Parsed ->
  Sem r (LetClause 'Scoped)
checkLetClause lc = case lc of
  LetTypeSig t -> LetTypeSig <$> checkLocalTypeSig t
  LetFunClause c -> LetFunClause <$> checkFunctionClause c

checkLetBlock ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
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
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda Lambda {..} = Lambda <$> mapM checkLambdaClause lambdaClauses

checkLambdaClause ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
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

checkUnqualified ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
  Symbol ->
  Sem r S.Name
checkUnqualified s = do
  -- Local vars have scope priority
  l <- HashMap.lookup s <$> asks _localVars
  case l of
    Just LocalVariable {..} -> return (unqualifiedSName variableName)
    Nothing -> do
      scope <- get
      locals <- ask
      -- Lookup at the global scope
      let err = throw (ErrSymNotInScope (NotInScope s locals scope))
      entries <- filter (S.isExprKind . S._nameKind) <$>
         lookupQualifiedSymbol ([], s)
      case entries of
        [] -> err
        [x] -> return (entryToSName (NameUnqualified s) x)
        es -> throw (ErrAmbiguousSym (AmbiguousSym es))

checkPatternName ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Name ->
  Sem r S.Name
checkPatternName n = do
  c <- isConstructor
  case c of
    Just constr -> return constr -- the symbol is a constructor
    Nothing -> unqualifiedSName <$> groupBindLocalVariable sym -- the symbol is a variable
  where
    (path, sym) = case n of
      NameQualified (QualifiedName (Path p) s) -> (toList p, s)
      NameUnqualified s -> ([], s)
    -- check whether the symbol is a constructor in scope
    isConstructor :: Sem r (Maybe S.Name)
    isConstructor = do
      entries <- filter (isConstructorKind . S._nameKind) <$>
          lookupQualifiedSymbol (path, sym)
      case entries of
            [] -> case Path <$> nonEmpty path of
              Nothing -> return Nothing -- There is no constructor with such a name
              Just pth -> throw (ErrQualSymNotInScope (QualifiedName pth sym))
            [e] -> return (Just (entryToSName n e)) -- There is one constructor with such a name
            _ -> throw $ ErrGeneric "There is more than one constructor with such a name"
    isConstructorKind :: S.NameKind -> Bool
    isConstructorKind = (== S.KNameConstructor)

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

-- | Binds a local variable in a bind group, i.e. a group of pattern.
groupBindLocalVariable ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
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
        (\x -> throw (ErrBindGroup
            BindGroupConflict {
                _bindGroupFirst = S._nameConcrete (variableName x),
                _bindGroupSecond =  s
                }))
    addToGroup :: Sem r S.Symbol
    addToGroup = do
      n <- freshVariable s
      modify (over scopeBindGroup (HashMap.insert s (LocalVariable n)))
      return n

checkPatternAtoms ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  PatternAtoms 'Parsed ->
  Sem r (PatternAtoms 'Scoped)
checkPatternAtoms (PatternAtoms s) = PatternAtoms <$> mapM checkPatternAtom s

checkPatternAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  PatternAtom 'Parsed ->
  Sem r (PatternAtom 'Scoped)
checkPatternAtom p = case p of
  PatternAtomWildcard -> return PatternAtomWildcard
  PatternAtomEmpty -> return PatternAtomEmpty
  PatternAtomParens e -> PatternAtomParens <$> checkPatternAtoms e
  PatternAtomName n -> PatternAtomName <$> checkPatternName n

checkName ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
  Name ->
  Sem r S.Name
checkName n = case n of
  NameQualified q -> checkQualifiedExpr q
  NameUnqualified s -> checkUnqualified s

checkExpressionAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  ExpressionAtom 'Parsed ->
  Sem r (ExpressionAtom 'Scoped)
checkExpressionAtom e = case e of
  AtomIdentifier n -> AtomIdentifier <$> checkName n
  AtomLambda lam -> AtomLambda <$> checkLambda lam
  AtomLetBlock letBlock -> AtomLetBlock <$> checkLetBlock letBlock
  AtomUniverse uni -> return (AtomUniverse uni)
  AtomFunction fun -> AtomFunction <$> checkFunction fun
  AtomParens par -> AtomParens <$> checkParens par
  AtomFunArrow -> return AtomFunArrow
  AtomMatch match -> AtomMatch <$> checkMatch match

checkParens :: Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  ExpressionAtoms 'Parsed -> Sem r Expression
checkParens e@(ExpressionAtoms as) = case as of
  AtomIdentifier s :| [] -> ExpressionParensIdentifier . set S.nameFixity Nothing <$> checkName s
  _ -> checkParseExpressionAtoms e

checkMatchAlt ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
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
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
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
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l) = ExpressionAtoms <$> mapM checkExpressionAtom l

checkParseExpressionAtoms ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  PatternAtom 'Parsed ->
  Sem r Pattern
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkStatement ::
  Members '[Error ScopeError, Reader ScopeParameters, Files, State Scope, State ScoperState, Reader LocalVars] r =>
  Statement 'Parsed ->
  Sem r (Statement 'Scoped)
checkStatement s = case s of
  StatementOperator opDef -> StatementOperator opDef <$ checkOperatorSyntaxDef opDef
  StatementTypeSignature tySig -> StatementTypeSignature <$> checkTypeSignature tySig
  StatementImport imp -> StatementImport <$> checkImport imp
  StatementInductive dt -> StatementInductive <$> checkInductiveDef dt
  StatementModule dt -> StatementModule <$> checkLocalModule dt
  StatementOpenModule open -> StatementOpenModule <$> checkOpenModule open
  StatementFunctionClause clause -> StatementFunctionClause <$> checkFunctionClause clause
  StatementAxiom ax -> StatementAxiom <$> checkAxiom ax
  StatementEval e -> StatementEval <$> checkEval e
  StatementPrint e -> StatementPrint <$> checkPrint e

-------------------------------------------------------------------------------
-- Infix Expression
-------------------------------------------------------------------------------
makeExpressionTable2 ::
  ExpressionAtoms 'Scoped -> [[P.Operator Parse Expression]]
makeExpressionTable2 (ExpressionAtoms atoms) = [appOp] : operators ++ [[functionOp]]
  where
  operators = mkSymbolTable names
  names :: [S.Name]
  names = mapMaybe getName (toList atoms)
    where
      getName :: ExpressionAtom 'Scoped -> Maybe S.Name
      getName a = case a of
        AtomIdentifier nm -> Just nm
        _ -> Nothing
  mkSymbolTable :: [S.Name] -> [[P.Operator Parse Expression]]
  mkSymbolTable = reverse . map (map snd) . groupSortOn fst . mapMaybe mkOperator
    where
      mkOperator :: S.Name -> Maybe (Precedence, P.Operator Parse Expression)
      mkOperator S.Name' {..}
        | Just Fixity {..} <- _nameFixity = Just $
          case fixityArity of
            Unary u -> (fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
              where
                unaryApp :: S.Name -> Expression -> Expression
                unaryApp funName arg = case u of
                  AssocPostfix -> ExpressionPostfixApplication (PostfixApplication arg funName)
            Binary b -> (fixityPrecedence, infixLRN (binaryApp <$> parseSymbolId _nameId))
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
  appOp = P.InfixL (return app)
    where
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
parseExpressionAtoms a@(ExpressionAtoms sections) = do
  case res of
    Left {} -> throw (ErrInfixParser
                       InfixError {_infixErrAtoms = a})
    Right r -> return r
  where
  parser :: Parse Expression
  parser = runM (mkExpressionParser tbl) <* P.eof
  res = P.parse parser filePath (toList sections)
  tbl = makeExpressionTable2 a
  filePath = ""

-- | Monad for parsing expression sections.
type Parse = P.Parsec () [ExpressionAtom 'Scoped]

mkExpressionParser ::
  [[P.Operator Parse Expression]] ->
  Sem '[Embed Parse] Expression
mkExpressionParser table = embed @Parse pExpression
  where
    pExpression :: Parse Expression
    pExpression = P.makeExprParser (runM parseTerm) table

parseTerm :: forall r. Members '[Embed Parse] r => Sem r Expression
parseTerm =
  embed @Parse $
    parseUniverse
      <|> parseNoInfixIdentifier
      <|> parseParens
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

    parseParens :: Parse Expression
    parseParens = P.token parenExpr mempty
      where
        parenExpr :: ExpressionAtom 'Scoped -> Maybe Expression
        parenExpr s = case s of
          AtomParens e -> Just e
          _ -> Nothing

-------------------------------------------------------------------------------
-- Infix Patterns
-------------------------------------------------------------------------------

type ParsePat = P.Parsec () [PatternAtom 'Scoped]

makePatternTable ::
  PatternAtom 'Scoped -> [[P.Operator ParsePat Pattern]]
makePatternTable atom = [appOp] : operators
  where
    operators = mkSymbolTable names
    names :: [S.Name]
    names = case atom of
      PatternAtomParens (PatternAtoms atoms) -> mapMaybe getName (toList atoms)
      _ -> []
      where
        getName :: PatternAtom 'Scoped -> Maybe S.Name
        getName a = case a of
          PatternAtomName nm -> Just nm
          _ -> Nothing
    mkSymbolTable :: [S.Name] -> [[P.Operator ParsePat Pattern]]
    mkSymbolTable = reverse . map (map snd) . groupSortOn fst . mapMaybe unqualifiedSymbolOp
      where
        unqualifiedSymbolOp :: S.Name -> Maybe (Precedence, P.Operator ParsePat Pattern)
        unqualifiedSymbolOp S.Name' {..}
          | Just Fixity {..} <- _nameFixity,
            _nameKind == S.KNameConstructor = Just $
            case fixityArity of
              Unary u -> (fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                where
                  unaryApp :: S.Name -> Pattern -> Pattern
                  unaryApp funName = case u of
                    AssocPostfix -> PatternPostfixApplication . (`PatternPostfixApp` funName)
              Binary b -> (fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId _nameId))
                where
                  binaryInfixApp :: S.Name -> Pattern -> Pattern -> Pattern
                  binaryInfixApp name argLeft = PatternInfixApplication . PatternInfixApp argLeft name
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
    appOp = P.InfixL (return app)
      where
        app :: Pattern -> Pattern -> Pattern
        app l = PatternApplication . PatternApp l

parsePatternTerm ::
  forall r.
  Members '[Reader (ParsePat Pattern), Embed ParsePat] r =>
  Sem r Pattern
parsePatternTerm = do
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
            | not (S.hasFixity n), S.isConstructor n -> Just n
          _ -> Nothing

    parseWildcard :: ParsePat Pattern
    parseWildcard = PatternWildcard <$ P.satisfy isWildcard
      where
        isWildcard :: PatternAtom 'Scoped -> Bool
        isWildcard s = case s of
          PatternAtomWildcard -> True
          _ -> False

    parseEmpty :: ParsePat Pattern
    parseEmpty = PatternEmpty <$ P.satisfy isEmpty
      where
        isEmpty :: PatternAtom 'Scoped -> Bool
        isEmpty s = case s of
          PatternAtomEmpty -> True
          _ -> False

    parseVariable :: ParsePat Pattern
    parseVariable = PatternVariable <$> P.token var mempty
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
        parseTermRec = runReader pPattern parsePatternTerm

parsePatternAtom ::
  Members '[Error ScopeError, State Scope] r => PatternAtom 'Scoped -> Sem r Pattern
parsePatternAtom sec = do
  case res of
    Left {} -> throw (ErrInfixPattern (InfixErrorP sec))
    Right r -> return r
  where
    tbl = makePatternTable sec
    parser :: ParsePat Pattern
    parser = runM (mkPatternParser tbl) <* P.eof
    res = P.parse parser filePath [sec]

    filePath = "tmp"
