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
import MiniJuvix.Syntax.Concrete.Parser (runModuleParserIO)
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import MiniJuvix.Syntax.Concrete.Scoped.Error
import MiniJuvix.Prelude
import qualified Data.List.NonEmpty as NonEmpty

--------------------------------------------------------------------------------

scopeCheck1 :: FilePath -> Module 'Parsed 'ModuleTop -> IO (Either ScopeError (Module 'Scoped 'ModuleTop))
scopeCheck1 root m = fmap head <$> scopeCheck root (pure m)

scopeCheck :: FilePath -> NonEmpty (Module 'Parsed 'ModuleTop) -> IO (Either ScopeError (NonEmpty (Module 'Scoped 'ModuleTop)))
scopeCheck root modules =
  runM $
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
symbolEntry S.Name' {..} = SymbolEntry
  { _symbolKind = _nameKind,
    _symbolDefinedIn = _nameDefinedIn,
    _symbolId = _nameId,
    _symbolFixity = _nameFixity,
    _symbolDefined = _nameDefined,
    _symbolPublicAnn = NoPublic,
    _symbolWhyInScope = BecauseDefined
  }

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
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, Embed IO, State ScoperState] r =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImport import_@(Import path) = do
  checkCycle
  cache <- gets (_cachedModules . _scoperModulesCache)
  entry' <- maybe (readParseModule path >>= local addImport . checkTopModule) return (cache ^. at path)
  let checked = _moduleEntryScoped entry'
      sname = modulePath checked
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
    { S.absTopModulePath = modulePath,
      S.absLocalPath = mempty
    }

-- | Looks for a symbol in (possibly) nested local modules
lookupSymbolGeneric :: forall a r. (Show a, Members '[State Scope, Error ScopeError, State ScoperState] r) =>
   (SymbolEntry -> Bool) -> a -> [Symbol] -> Symbol -> Sem r [SymbolEntry]
lookupSymbolGeneric filtr name modules final = do
  local' <- inLocalModule
  import' <- importedTopModule
  return $ maybeToList local' ++ maybeToList import'
  where
  inLocalModule :: Sem r (Maybe SymbolEntry) =
    case modules of
      [] -> do
         r <- HashMap.lookup final <$> gets _scopeSymbols
         case r of
          Nothing -> return Nothing
          Just SymbolInfo {..} -> case filter filtr (toList _symbolInfo) of
            [] -> return Nothing
            [e] -> return (Just e)
            es -> throw (ErrAmbiguousSym es) -- This is meant to happen only at the top level
      (p : ps) -> do
        r <- fmap (filter (S.isModuleKind . _symbolKind) . toList . _symbolInfo)
          . HashMap.lookup p <$> gets _scopeSymbols
        case r of
          Just [x] -> do
            export <- getExportInfo (_symbolId x)
            lookInExport final ps export
          Just [] -> return Nothing
          Just _ -> throw $ ErrGeneric ("ambiguous name " <> show name)
          Nothing -> return Nothing
  importedTopModule :: Sem r (Maybe SymbolEntry)
  importedTopModule = do
    fmap mkEntry . HashMap.lookup path <$> gets _scopeTopModules
    where
    mkEntry modId = SymbolEntry {
         _symbolKind = S.KNameTopModule ,
         _symbolDefinedIn = S.topModulePathToAbsPath path,
         _symbolDefined = getLoc final,
         _symbolId = modId,
         _symbolFixity = S.NoFixity,
         _symbolWhyInScope = BecauseImportedOpened,
         _symbolPublicAnn = NoPublic}
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
      guard (S.isModuleKind (_symbolKind entry))
      return entry of
        Just entry -> Just <$> getExportInfo (_symbolId entry)
        Nothing -> return Nothing

-- | We return a list of entries because qualified names can point to different
-- modules due to nesting.
lookupQualifiedSymbol :: forall r. Members '[State Scope, Error ScopeError, State ScoperState] r =>
   ([Symbol], Symbol) -> Sem r [SymbolEntry]
lookupQualifiedSymbol q@(path, sym) = do
  here' <- here
  there' <- there
  return (here' ++ there')
  where
  -- | Looks for a local module symbol in scope
  here :: Sem r [SymbolEntry]
  here = lookupSymbolGeneric (S.isModuleKind . _symbolKind) q path sym
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
     [e] -> return $ entryToSName q' e
     _ -> throw (ErrAmbiguousSym es)
  where
  q' = NameQualified q
  notInScope = throw (ErrQualSymNotInScope q)


unqualifiedSName :: S.Symbol -> S.Name
unqualifiedSName = over S.nameConcrete NameUnqualified

entryToSName :: s -> SymbolEntry -> S.Name' s
entryToSName s SymbolEntry {..} =
  S.Name'
    { _nameId = _symbolId,
      _nameConcrete = s,
      _nameDefinedIn = _symbolDefinedIn,
      _nameDefined = _symbolDefined,
      _nameFixity = _symbolFixity,
      _nameKind = _symbolKind
    }

-- | We gather all symbols which have been defined or marked to be public in the given scope.
exportScope :: forall r. Members '[Error ScopeError] r => Scope -> Sem r ExportInfo
exportScope Scope {..} = do
  _exportSymbols <- getExportSymbols
  return ExportInfo {..}
  where
    getExportSymbols :: Sem r (HashMap Symbol SymbolEntry)
    getExportSymbols = HashMap.fromList <$> mapMaybeM entry (HashMap.toList _scopeSymbols)
      where
        shouldExport :: SymbolEntry -> Bool
        shouldExport SymbolEntry {..} =
          _symbolDefinedIn == _scopePath
            || _symbolPublicAnn == Public
        entry :: (Symbol, SymbolInfo) -> Sem r (Maybe (Symbol, SymbolEntry))
        entry (s, SymbolInfo {..}) =
          case filter shouldExport (toList _symbolInfo) of
            [] -> return Nothing
            [e] -> return $ Just (s, e)
            _ -> throw (ErrMultipleExport s)

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
  return $ topModulePathToFilePath' (Just ext) root mp

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
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature TypeSignature {..} = do
  sigType' <- checkParseExpressionAtoms sigType
  sigName' <- bindFunctionSymbol sigName
  return
    TypeSignature
      { sigName = sigName',
        sigType = sigType'
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
        inductiveParameterType' <- checkParseExpressionAtoms inductiveParameterType
        inductiveParameterName' <- freshVariable inductiveParameterName
        let param' =
              InductiveParameter
                { inductiveParameterType = inductiveParameterType',
                  inductiveParameterName = inductiveParameterName'
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
  withParams inductiveParameters $ \inductiveParameters' -> do
    inductiveType' <- sequence (checkParseExpressionAtoms <$> inductiveType)
    inductiveName' <- bindInductiveSymbol inductiveName
    inductiveConstructors' <- mapM checkConstructorDef inductiveConstructors
    return InductiveDef
      { inductiveName = inductiveName',
        inductiveParameters = inductiveParameters',
        inductiveType = inductiveType',
        inductiveConstructors = inductiveConstructors'
      }

checkTopModule_ ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO, State ScoperState] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule_ = fmap _moduleEntryScoped . checkTopModule

checkTopModule ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO, State ScoperState] r =>
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
        _nameFixity = S.NoFixity
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
          modulePath = path',
          moduleParameters = params',
          moduleBody = body' }
        return ModuleEntry' {..}

withScope :: Members '[State Scope] r => Sem r a -> Sem r a
withScope ma = do
  before <- get @Scope
  x <- ma
  put before
  return x

checkModuleBody :: forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScoperState, Embed IO, Reader LocalVars] r =>
  [Statement 'Parsed] ->
  Sem r (ExportInfo, [Statement 'Scoped])
checkModuleBody body = do
  body' <- mapM checkStatement body
  checkOrphanFixities
  exported <- get >>= exportScope
  return (exported, body')

checkLocalModule ::
  forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScoperState, Embed IO, Reader LocalVars] r =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule Module {..} = do
  (_moduleEntryExport, moduleBody', moduleParameters') <-
      withScope $ withParams moduleParameters $ \p' -> do
    inheritScope
    (e, b) <- checkModuleBody moduleBody
    return (e, b, p')
  modulePath' <- bindLocalModuleSymbol modulePath
  let moduleId = S._nameId modulePath'
      _moduleEntryScoped = Module
        { modulePath = modulePath',
          moduleParameters = moduleParameters',
          moduleBody = moduleBody'
        }
      entry = mkModuleEntry ModuleEntry' {..}
  modify (over scoperModules (HashMap.insert moduleId entry))
  return _moduleEntryScoped
  where
  inheritScope :: Sem r ()
  inheritScope = do
    absPath <- (S.<.> modulePath) <$> gets _scopePath
    modify (set scopePath absPath)
    modify (over scopeSymbols (fmap inheritSymbol))
    modify (set scopeFixities mempty) -- do not inherit fixity declarations
    where
    inheritSymbol :: SymbolInfo -> SymbolInfo
    inheritSymbol (SymbolInfo s) = SymbolInfo (fmap inheritEntry s)
    inheritEntry :: SymbolEntry -> SymbolEntry
    inheritEntry = over symbolWhyInScope BecauseInherited

-- | TODO checks if there is an infix declaration without a binding.
checkOrphanFixities :: Members '[Error ScopeError, State Scope] r => Sem r ()
checkOrphanFixities = return ()

symbolInfoSingle :: SymbolEntry -> SymbolInfo
symbolInfoSingle p = SymbolInfo $ HashMap.singleton (_symbolDefinedIn p) p

lookupModuleSymbol :: Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Name -> Sem r S.Name
lookupModuleSymbol n = do
  es <- lookupQualifiedSymbol (path, sym)
  case filter (S.isModuleKind . _symbolKind) es of
    [] -> notInScope
    [x] -> return $ entryToSName n x
    _ -> throw (ErrAmbiguousModuleSym es)
  where
  notInScope = throw (ErrModuleNotInScope n)
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
        alterEntry = set symbolWhyInScope BecauseImportedOpened . set symbolPublicAnn openPublic
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
    clp <- mapM checkParsePatternAtom clausePatterns
    withBindCurrentGroup $ do
      s <- get @Scope
      clw <- sequence (checkWhereBlock <$> clauseWhere)
      clb <- checkParseExpressionAtoms clauseBody
      put s
      return (clp, clw, clb)
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
      e@SymbolEntry {..} <- maybe err return (HashMap.lookup path _symbolInfo)
      when (_symbolKind /= S.KNameFunction) err
      return (entryToSName fun e)
      where
        err :: Sem r a
        err = throw (ErrLacksTypeSig (LacksTypeSig clause))

checkAxiom ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
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
      entryToSName (NameUnqualified s) <$>
         -- TODO change listToMaybe, it is a bit too hacky
        fromMaybeM err (listToMaybe <$> lookupSymbolGeneric (S.isExprKind . _symbolKind) s [] s)


checkPatternName ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
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
        (const (throw (ErrBindGroup s)))
    addToGroup :: Sem r S.Symbol
    addToGroup = do
      n <- freshVariable s
      modify (over scopeBindGroup (HashMap.insert s (LocalVariable n)))
      return n

checkPatternUnqualified ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Symbol ->
  Sem r S.Name
checkPatternUnqualified s = do
  c <- isConstructor
  case c of
    Just constr -> return constr -- the symbol is a constructor
    Nothing -> unqualifiedSName <$> groupBindLocalVariable s -- the symbol is a variable
  where
    -- check whether the symbol is a constructor in scope
    isConstructor :: Sem r (Maybe S.Name)
    isConstructor = do
      r <- HashMap.lookup s <$> gets _scopeSymbols
      case r of
        Nothing -> return Nothing
        Just SymbolInfo {..} ->
          let entries = filter (isConstructorKind . _symbolKind . snd) (HashMap.toList _symbolInfo)
           in case map snd entries of
                [] -> return Nothing -- There is no constructor with such a name
                [e] -> return (Just (entryToSName (NameUnqualified s) e)) -- There is one constructor with such a name
                _ -> throw $ ErrGeneric "There is more than one constructor with such a name"
    isConstructorKind :: S.NameKind -> Bool
    isConstructorKind = (== S.KNameConstructor)

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
  AtomIdentifier s :| [] -> ExpressionParensIdentifier . set S.nameFixity S.NoFixity <$> checkName s
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
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO, State Scope, State ScoperState, Reader LocalVars] r =>
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
    mkSymbolTable = map (map snd) . groupSortOn fst . mapMaybe (getEntry >=> unqualifiedSymbolOp)
      where
        getEntry :: SymbolInfo -> Maybe SymbolEntry
        getEntry (SymbolInfo m) = case toList m of
          [] -> impossible
          [e] -> Just e
          _ -> Nothing -- ambiguous symbol, will result in an error if found
        unqualifiedSymbolOp :: SymbolEntry -> Maybe (Precedence, P.Operator Parse Expression)
        unqualifiedSymbolOp SymbolEntry {..}
          | S.SomeFixity Fixity {..} <- _symbolFixity = Just $
            case fixityArity of
              Unary u -> (fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _symbolId))
                where
                  unaryApp :: S.Name -> Expression -> Expression
                  unaryApp funName arg = case u of
                    AssocPostfix -> ExpressionPostfixApplication (PostfixApplication arg funName)
              Binary b -> (fixityPrecedence, infixLRN (binaryApp <$> parseSymbolId _symbolId))
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
parseExpressionAtoms a@(ExpressionAtoms sections) = do
  tbl <- makeExpressionTable
  let parser :: Parse Expression
      parser = runM (mkExpressionParser tbl) <* P.eof
      res = P.parse parser filePath (toList sections)
  case res of
    Left {} -> throw (ErrInfixParser
                       InfixError {
                        _infixErrAtoms = a
          })
    Right r -> return r
  where
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
    mkSymbolTable = map (map snd) . groupSortOn fst . mapMaybe (getEntry >=> unqualifiedSymbolOp)
      where
        getEntry :: SymbolInfo -> Maybe SymbolEntry
        getEntry (SymbolInfo m) = case toList m of
          [] -> impossible
          [e] -> Just e
          _ -> Nothing -- if this symbol es found will result in an ambiguity error.
        unqualifiedSymbolOp :: SymbolEntry -> Maybe (Precedence, P.Operator ParsePat Pattern)
        unqualifiedSymbolOp SymbolEntry {..}
          | S.SomeFixity Fixity {..} <- _symbolFixity,
            _symbolKind == S.KNameConstructor = Just $
            case fixityArity of
              Unary u -> (fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _symbolId))
                where
                  unaryApp :: S.Name -> Pattern -> Pattern
                  unaryApp funName = case u of
                    AssocPostfix -> PatternPostfixApplication . (`PatternPostfixApp` funName)
              Binary b -> (fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId _symbolId))
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
  tbl <- makePatternTable
  let parser :: ParsePat Pattern
      parser = runM (mkPatternParser tbl) <* P.eof
      res = P.parse parser filePath [sec]
  case res of
    Left {} -> throw (ErrInfixPattern (InfixErrorP sec))
    Right r -> return r
  where
    filePath = "tmp"
