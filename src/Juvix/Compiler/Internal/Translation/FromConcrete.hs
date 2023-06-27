module Juvix.Compiler.Internal.Translation.FromConcrete
  ( module Juvix.Compiler.Internal.Translation.FromConcrete,
    module Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Abstract.Data.NameDependencyInfo qualified as Abstract
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra qualified as Concrete
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Internal.Extra.DependencyBuilder
import Juvix.Compiler.Internal.Language (varFromWildcard)
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Data.NameKind
import Juvix.Prelude

newtype TranslationState = TranslationState
  { -- | Top modules are supposed to be included at most once.
    _translationStateIncluded :: HashSet S.TopModulePath
  }

iniState :: TranslationState
iniState =
  TranslationState
    { _translationStateIncluded = mempty
    }

makeLenses ''TranslationState

unsupported :: Text -> a
unsupported msg = error $ msg <> "Scoped to Internal: not yet supported"

fromConcrete ::
  Members '[Error JuvixError, Builtins, NameIdGen] r =>
  Scoper.ScoperResult ->
  Sem r InternalResult
fromConcrete _resultScoper =
  mapError (JuvixError @ScoperError) $ do
    (_resultModulesCache, _resultModules) <-
      runReader @Pragmas mempty
        . runReader @ExportsTable exportTbl
        . runState (ModulesCache mempty)
        . evalState iniState
        $ mapM goTopModule ms
    let _resultTable = buildTable _resultModules
        _resultDepInfo = buildDependencyInfo _resultModules exportTbl
    return InternalResult {..}
  where
    ms = _resultScoper ^. Scoper.resultModules
    exportTbl = _resultScoper ^. Scoper.resultExports

-- | `StatementInclude`s are no included in the result
buildMutualBlocks ::
  Members '[Reader Abstract.NameDependencyInfo] r =>
  [Internal.PreStatement] ->
  Sem r [SCC Internal.PreStatement]
buildMutualBlocks ss = do
  depInfo <- ask
  let scomponents :: [SCC Internal.Name] = buildSCCs depInfo
  return (boolHack (mapMaybe nameToPreStatement scomponents))
  where
    -- If the builtin bool definition is found, it is moved at the front.
    --
    -- This is a hack needed to translate BuiltinStringToNat in
    -- internal-to-core. BuiltinStringToNat is the only function that depends on
    -- Bool implicitly (i.e. without mentioning it in its type). Eventually
    -- BuiltinStringToNat needs to be removed and so this hack.
    boolHack :: [SCC Internal.PreStatement] -> [SCC Internal.PreStatement]
    boolHack s = case popFirstJust isBuiltinBool s of
      (Nothing, _) -> s
      (Just boolDef, rest) -> AcyclicSCC (Internal.PreInductiveDef boolDef) : rest
      where
        isBuiltinBool :: SCC Internal.PreStatement -> Maybe Internal.InductiveDef
        isBuiltinBool = \case
          CyclicSCC [Internal.PreInductiveDef b]
            | Just BuiltinBool <- b ^. Internal.inductiveBuiltin -> Just b
          _ -> Nothing

    statementsByName :: HashMap Internal.Name Internal.PreStatement
    statementsByName = HashMap.fromList (map mkAssoc ss)
      where
        mkAssoc :: Internal.PreStatement -> (Internal.Name, Internal.PreStatement)
        mkAssoc s = case s of
          Internal.PreInductiveDef i -> (i ^. Internal.inductiveName, s)
          Internal.PreFunctionDef i -> (i ^. Internal.funDefName, s)
          Internal.PreAxiomDef i -> (i ^. Internal.axiomName, s)

    getStmt :: Internal.Name -> Maybe Internal.PreStatement
    getStmt n = statementsByName ^. at n

    nameToPreStatement :: SCC Internal.Name -> Maybe (SCC Internal.PreStatement)
    nameToPreStatement = nonEmptySCC . fmap getStmt
      where
        nonEmptySCC :: SCC (Maybe a) -> Maybe (SCC a)
        nonEmptySCC = \case
          AcyclicSCC a -> AcyclicSCC <$> a
          CyclicSCC p -> CyclicSCC . toList <$> nonEmpty (catMaybes p)

buildLetMutualBlocks ::
  Members '[Reader Abstract.NameDependencyInfo] r =>
  [Internal.FunctionDef] ->
  Sem r [SCC Internal.FunctionDef]
buildLetMutualBlocks = fmap (map (fmap fromStmt)) . buildMutualBlocks . map Internal.PreFunctionDef
  where
    fromStmt :: Internal.PreStatement -> Internal.FunctionDef
    fromStmt = \case
      Internal.PreFunctionDef f -> f
      _ -> impossible

fromConcreteExpression :: (Members '[Error JuvixError, NameIdGen] r) => Scoper.Expression -> Sem r Internal.Expression
fromConcreteExpression = mapError (JuvixError @ScoperError) . runReader @Pragmas mempty . goExpression

fromConcreteImport ::
  Members '[Reader ExportsTable, Error JuvixError, NameIdGen, Builtins, State ModulesCache, State TranslationState] r =>
  Scoper.Import 'Scoped ->
  Sem r (Maybe Internal.Include)
fromConcreteImport =
  mapError (JuvixError @ScoperError)
    . runReader @Pragmas mempty
    . goImport

fromConcreteOpenImport ::
  Members '[Reader ExportsTable, Error JuvixError, NameIdGen, Builtins, State ModulesCache, State TranslationState] r =>
  Scoper.OpenModule 'Scoped ->
  Sem r (Maybe Internal.Include)
fromConcreteOpenImport = mapError (JuvixError @ScoperError) . runReader @Pragmas mempty . goOpenModule'

-- | returns (cacheHit, module)
goTopModule ::
  forall r.
  (Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r) =>
  Module 'Scoped 'ModuleTop ->
  Sem r Internal.Module
goTopModule m = do
  let moduleNameId :: S.NameId
      moduleNameId = m ^. Concrete.modulePath . S.nameId
      processModule :: Sem r Internal.Module
      processModule = do
        am <- goModule' m
        modify (over cachedModules (HashMap.insert moduleNameId am))
        return am
  cache <- gets (^. cachedModules)
  maybe processModule return (cache ^. at moduleNameId)

goLocalModule ::
  Members '[Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r =>
  Module 'Scoped 'ModuleLocal ->
  Sem r [Internal.PreStatement]
goLocalModule = concatMapM goStatement . (^. moduleBody)

goModule' ::
  forall r t.
  (Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r, SingI t) =>
  Module 'Scoped t ->
  Sem r Internal.Module
goModule' m = do
  p <- toPreModule m
  tbl <- ask
  let depInfo = buildDependencyInfoPreModule p tbl
  runReader depInfo (fromPreModule p)

goPragmas :: Member (Reader Pragmas) r => Maybe ParsedPragmas -> Sem r Pragmas
goPragmas p = do
  p' <- ask
  return $ p' <> p ^. _Just . withLocParam . withSourceValue

goName :: S.Name -> Internal.Name
goName name =
  set Internal.namePretty prettyStr (goSymbol (S.nameUnqualify name))
  where
    prettyStr :: Text
    prettyStr = prettyText name

goSymbol :: S.Symbol -> Internal.Name
goSymbol s =
  Internal.Name
    { _nameText = S.symbolText s,
      _nameId = s ^. S.nameId,
      _nameKind = getNameKind s,
      _namePretty = S.symbolText s,
      _nameLoc = s ^. S.nameConcrete . symbolLoc,
      _nameFixity = s ^. S.nameFixity
    }

-- TODO give a better name?
traverseM' ::
  forall r s t a b.
  (Monad r, Monad s, Traversable t) =>
  (a -> r (s b)) ->
  t a ->
  r (s (t b))
traverseM' f x = sequence <$> traverse f x

toPreModule ::
  forall r t.
  (SingI t, Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r) =>
  Module 'Scoped t ->
  Sem r Internal.PreModule
toPreModule Module {..} = do
  pragmas' <- goPragmas _modulePragmas
  body' <- local (const pragmas') (goModuleBody _moduleBody)
  examples' <- goExamples _moduleDoc
  return
    Internal.Module
      { _moduleName = name',
        _moduleBody = body',
        _moduleExamples = examples',
        _modulePragmas = pragmas'
      }
  where
    name' :: Internal.Name
    name' = case sing :: SModuleIsTop t of
      SModuleTop -> goSymbol (S.topModulePathName _modulePath)
      SModuleLocal -> goSymbol _modulePath

fromPreModule ::
  forall r.
  Members '[Reader Abstract.NameDependencyInfo, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r =>
  Internal.PreModule ->
  Sem r Internal.Module
fromPreModule = traverseOf Internal.moduleBody fromPreModuleBody

fromPreModuleBody ::
  forall r.
  Members '[Reader Abstract.NameDependencyInfo, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r =>
  Internal.PreModuleBody ->
  Sem r Internal.ModuleBody
fromPreModuleBody b = do
  sccs <- buildMutualBlocks (b ^. Internal.moduleStatements)
  let moduleStatements' = map goSCC sccs
  return (set Internal.moduleStatements moduleStatements' b)
  where
    goSCC :: SCC Internal.PreStatement -> Internal.Statement
    goSCC = \case
      AcyclicSCC s -> goAcyclic s
      CyclicSCC c -> goCyclic (nonEmpty' c)
      where
        goCyclic :: NonEmpty Internal.PreStatement -> Internal.Statement
        goCyclic c = Internal.StatementMutual (Internal.MutualBlock (goMutual <$> c))
          where
            goMutual :: Internal.PreStatement -> Internal.MutualStatement
            goMutual = \case
              Internal.PreInductiveDef i -> Internal.StatementInductive i
              Internal.PreFunctionDef i -> Internal.StatementFunction i
              _ -> impossible

        goAcyclic :: Internal.PreStatement -> Internal.Statement
        goAcyclic = \case
          Internal.PreInductiveDef i -> one (Internal.StatementInductive i)
          Internal.PreFunctionDef i -> one (Internal.StatementFunction i)
          Internal.PreAxiomDef i -> Internal.StatementAxiom i
          where
            one :: Internal.MutualStatement -> Internal.Statement
            one = Internal.StatementMutual . Internal.MutualBlock . pure

goModuleBody ::
  forall r.
  (Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r) =>
  [Statement 'Scoped] ->
  Sem r Internal.PreModuleBody
goModuleBody stmts = do
  _moduleIncludes <- mapMaybeM goImport (scanImports stmts)
  otherThanFunctions :: [Indexed Internal.PreStatement] <- concatMapM (traverseM' goStatement) ss
  functions <- map (fmap Internal.PreFunctionDef) <$> compiledFunctions
  let _moduleStatements =
        map
          (^. indexedThing)
          ( sortOn
              (^. indexedIx)
              (otherThanFunctions <> functions)
          )
  return Internal.ModuleBody {..}
  where
    ss' = concatMap Concrete.flattenStatement stmts

    ss :: [Indexed (Statement 'Scoped)]
    ss = zipWith Indexed [0 ..] ss'

    compiledFunctions :: Sem r [Indexed Internal.FunctionDef]
    compiledFunctions =
      sequence
        [ Indexed i <$> funDef
          | Indexed i sig <- sigs,
            let name = sig ^. sigName,
            let funDef = goTopFunctionDef sig (getClauses name)
        ]
      where
        getClauses :: S.Symbol -> [FunctionClause 'Scoped]
        getClauses name = [c | StatementFunctionClause c <- ss', name == c ^. clauseOwnerFunction]
        sigs :: [Indexed (TypeSignature 'Scoped)]
        sigs = [Indexed i t | (Indexed i (StatementTypeSignature t)) <- ss]

scanImports :: [Statement 'Scoped] -> [Import 'Scoped]
scanImports stmts = mconcatMap go stmts
  where
    go :: Statement 'Scoped -> [Import 'Scoped]
    go = \case
      StatementImport t -> [t]
      StatementModule m -> concatMap go (m ^. moduleBody)
      StatementOpenModule o -> maybeToList (openImport o)
      StatementInductive {} -> []
      StatementFunctionClause {} -> []
      StatementTypeSignature {} -> []
      StatementAxiom {} -> []
      StatementSyntax {} -> []
      where
        openImport :: OpenModule 'Scoped -> Maybe (Import 'Scoped)
        openImport o = case o ^. openModuleImportKw of
          Nothing -> Nothing
          Just _importKw ->
            Just
              Import
                { _importModule = case o ^. openModuleName . unModuleRef' of
                    SModuleTop :&: r -> r
                    SModuleLocal :&: _ -> impossible,
                  _importAsName = o ^. openImportAsName,
                  _importKw
                }

goImport ::
  forall r.
  Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, State ModulesCache, Reader Pragmas, State TranslationState] r =>
  Import 'Scoped ->
  Sem r (Maybe Internal.Include)
goImport Import {..} = do
  -- TOOD cache???
  -- guardNotCached <$> goTopModule (_importModule ^. moduleRefModule)
  let m = _importModule ^. moduleRefModule
      mname = m ^. Concrete.modulePath
  inc <- gets (HashSet.member mname . (^. translationStateIncluded))
  if
      | inc -> return Nothing
      | otherwise -> do
          modify (over translationStateIncluded (HashSet.insert mname))
          m' <- goTopModule m
          return
            ( Just
                Internal.Include
                  { _includeModule = m'
                  }
            )

guardNotCached :: (Bool, Internal.Module) -> Maybe Internal.Module
guardNotCached (hit, m) = do
  guard (not hit)
  return m

goStatement ::
  forall r.
  Members '[Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r =>
  Statement 'Scoped ->
  Sem r [Internal.PreStatement]
goStatement = \case
  StatementInductive i -> pure . Internal.PreInductiveDef <$> goInductive i
  StatementAxiom d -> pure . Internal.PreAxiomDef <$> goAxiom d
  StatementModule f -> goLocalModule f
  StatementImport {} -> return []
  StatementSyntax {} -> return []
  StatementOpenModule {} -> return []
  StatementTypeSignature {} -> return []
  StatementFunctionClause {} -> return []

goOpenModule' ::
  forall r.
  Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, State ModulesCache, Reader Pragmas, State TranslationState] r =>
  OpenModule 'Scoped ->
  Sem r (Maybe Internal.Include)
goOpenModule' o =
  case o ^. openModuleImportKw of
    Just kw ->
      case o ^. openModuleName of
        ModuleRef' (SModuleTop :&: m) ->
          goImport
            Import
              { _importKw = kw,
                _importModule = m,
                _importAsName = o ^. openImportAsName
              }
        _ -> impossible
    Nothing -> return Nothing

goOpenModule ::
  forall r.
  Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r =>
  OpenModule 'Scoped ->
  Sem r (Maybe Internal.Include)
goOpenModule o = goOpenModule' o

goLetFunctionDef ::
  Members '[NameIdGen, Reader Pragmas, Error ScoperError] r =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Internal.FunctionDef
goLetFunctionDef = goFunctionDefHelper

goFunctionDefHelper ::
  forall r.
  Members '[NameIdGen, Reader Pragmas, Error ScoperError] r =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Internal.FunctionDef
goFunctionDefHelper sig@TypeSignature {..} clauses = do
  let _funDefName = goSymbol _sigName
      _funDefTerminating = isJust _sigTerminating
      _funDefBuiltin = (^. withLocParam) <$> _sigBuiltin
  _funDefType <- goExpression _sigType
  _funDefExamples <- goExamples _sigDoc
  _funDefPragmas <- goPragmas _sigPragmas
  _funDefClauses <- case (_sigBody, nonEmpty clauses) of
    (Nothing, Nothing) -> throw (ErrLacksFunctionClause (LacksFunctionClause sig))
    (Just {}, Just clauses') -> throw (ErrDuplicateFunctionClause (DuplicateFunctionClause sig (head clauses')))
    (Just body, Nothing) -> do
      body' <- goExpression body
      return
        ( pure
            Internal.FunctionClause
              { _clauseName = _funDefName,
                _clausePatterns = [],
                _clauseBody = body'
              }
        )
    (Nothing, Just clauses') -> mapM goFunctionClause clauses'
  return Internal.FunctionDef {..}

goTopFunctionDef ::
  forall r.
  (Members '[Reader Pragmas, Error ScoperError, Builtins, NameIdGen] r) =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Internal.FunctionDef
goTopFunctionDef sig clauses = do
  fun <- goFunctionDefHelper sig clauses
  whenJust (sig ^. sigBuiltin) (registerBuiltinFunction fun . (^. withLocParam))
  return fun

goExamples ::
  forall r.
  Members '[NameIdGen, Error ScoperError, Reader Pragmas] r =>
  Maybe (Judoc 'Scoped) ->
  Sem r [Internal.Example]
goExamples = mapM goExample . maybe [] judocExamples
  where
    goExample :: Example 'Scoped -> Sem r Internal.Example
    goExample ex = do
      e' <- goExpression (ex ^. exampleExpression)
      return
        Internal.Example
          { _exampleExpression = e',
            _exampleId = ex ^. exampleId
          }

goFunctionClause ::
  Members '[NameIdGen, Error ScoperError, Reader Pragmas] r =>
  FunctionClause 'Scoped ->
  Sem r Internal.FunctionClause
goFunctionClause FunctionClause {..} = do
  _clausePatterns' <- mapM goPatternArg _clausePatterns
  _clauseBody' <- goExpression _clauseBody
  return
    Internal.FunctionClause
      { _clauseName = goSymbol _clauseOwnerFunction,
        _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goInductiveParameters ::
  Members '[NameIdGen, Error ScoperError, Reader Pragmas] r =>
  InductiveParameters 'Scoped ->
  Sem r [Internal.InductiveParameter]
goInductiveParameters InductiveParameters {..} = do
  paramType' <- goExpression _inductiveParametersType
  case paramType' of
    Internal.ExpressionUniverse {} -> return ()
    _ -> unsupported "only type variables of small types are allowed"

  let goInductiveParameter :: S.Symbol -> Internal.InductiveParameter
      goInductiveParameter var =
        Internal.InductiveParameter
          { _inductiveParamName = goSymbol var
          }
  return (map goInductiveParameter (toList _inductiveParametersNames))

registerBuiltinInductive ::
  (Members '[Error ScoperError, Builtins] r) =>
  Internal.InductiveDef ->
  BuiltinInductive ->
  Sem r ()
registerBuiltinInductive d = \case
  BuiltinNat -> registerNatDef d
  BuiltinBool -> registerBoolDef d
  BuiltinInt -> registerIntDef d

registerBuiltinFunction ::
  (Members '[Error ScoperError, Builtins, NameIdGen] r) =>
  Internal.FunctionDef ->
  BuiltinFunction ->
  Sem r ()
registerBuiltinFunction d = \case
  BuiltinNatPlus -> registerNatPlus d
  BuiltinNatSub -> registerNatSub d
  BuiltinNatMul -> registerNatMul d
  BuiltinNatUDiv -> registerNatUDiv d
  BuiltinNatDiv -> registerNatDiv d
  BuiltinNatMod -> registerNatMod d
  BuiltinNatLe -> registerNatLe d
  BuiltinNatLt -> registerNatLt d
  BuiltinNatEq -> registerNatEq d
  BuiltinBoolIf -> registerIf d
  BuiltinBoolOr -> registerOr d
  BuiltinBoolAnd -> registerAnd d
  BuiltinIntEq -> registerIntEq d
  BuiltinIntSubNat -> registerIntSubNat d
  BuiltinIntPlus -> registerIntPlus d
  BuiltinIntNegNat -> registerIntNegNat d
  BuiltinIntNeg -> registerIntNeg d
  BuiltinIntMul -> registerIntMul d
  BuiltinIntDiv -> registerIntDiv d
  BuiltinIntMod -> registerIntMod d
  BuiltinIntSub -> registerIntSub d
  BuiltinIntNonNeg -> registerIntNonNeg d
  BuiltinIntLe -> registerIntLe d
  BuiltinIntLt -> registerIntLt d
  BuiltinSeq -> registerSeq d

registerBuiltinAxiom ::
  (Members '[Error ScoperError, Builtins, NameIdGen] r) =>
  Internal.AxiomDef ->
  BuiltinAxiom ->
  Sem r ()
registerBuiltinAxiom d = \case
  BuiltinIO -> registerIO d
  BuiltinIOSequence -> registerIOSequence d
  BuiltinIOReadline -> registerIOReadline d
  BuiltinNatPrint -> registerNatPrint d
  BuiltinNatToString -> registerNatToString d
  BuiltinString -> registerString d
  BuiltinStringPrint -> registerStringPrint d
  BuiltinStringConcat -> registerStringConcat d
  BuiltinStringEq -> registerStringEq d
  BuiltinStringToNat -> registerStringToNat d
  BuiltinBoolPrint -> registerBoolPrint d
  BuiltinTrace -> registerTrace d
  BuiltinFail -> registerFail d
  BuiltinIntToString -> registerIntToString d
  BuiltinIntPrint -> registerIntPrint d

goInductive ::
  Members '[NameIdGen, Reader Pragmas, Builtins, Error ScoperError] r =>
  InductiveDef 'Scoped ->
  Sem r Internal.InductiveDef
goInductive ty@InductiveDef {..} = do
  _inductiveParameters' <- concatMapM goInductiveParameters _inductiveParameters
  _inductiveType' <- mapM goExpression _inductiveType
  _inductivePragmas' <- goPragmas _inductivePragmas
  _inductiveConstructors' <-
    local (const _inductivePragmas') $
      mapM goConstructorDef _inductiveConstructors
  _inductiveExamples' <- goExamples _inductiveDoc
  let loc = getLoc _inductiveName
      indDef =
        Internal.InductiveDef
          { _inductiveParameters = _inductiveParameters',
            _inductiveBuiltin = (^. withLocParam) <$> _inductiveBuiltin,
            _inductiveName = goSymbol _inductiveName,
            _inductiveType = fromMaybe (Internal.ExpressionUniverse (SmallUniverse loc)) _inductiveType',
            _inductiveConstructors = toList _inductiveConstructors',
            _inductiveExamples = _inductiveExamples',
            _inductivePragmas = _inductivePragmas',
            _inductivePositive = isJust (ty ^. inductivePositive)
          }
  whenJust ((^. withLocParam) <$> _inductiveBuiltin) (registerBuiltinInductive indDef)
  return indDef

goConstructorDef ::
  Members [NameIdGen, Error ScoperError, Reader Pragmas] r =>
  InductiveConstructorDef 'Scoped ->
  Sem r Internal.InductiveConstructorDef
goConstructorDef InductiveConstructorDef {..} = do
  ty' <- goExpression _constructorType
  examples' <- goExamples _constructorDoc
  pragmas' <- goPragmas _constructorPragmas
  return
    Internal.InductiveConstructorDef
      { _inductiveConstructorType = ty',
        _inductiveConstructorExamples = examples',
        _inductiveConstructorName = goSymbol _constructorName,
        _inductiveConstructorPragmas = pragmas'
      }

goLiteral :: LiteralLoc -> Internal.LiteralLoc
goLiteral = fmap go
  where
    go :: Literal -> Internal.Literal
    go = \case
      LitString s -> Internal.LitString s
      LitInteger i -> Internal.LitInteger i

goExpression ::
  forall r.
  Members [NameIdGen, Error ScoperError, Reader Pragmas] r =>
  Expression ->
  Sem r Internal.Expression
goExpression = \case
  ExpressionIdentifier nt -> return (goIden nt)
  ExpressionParensIdentifier nt -> return (goIden nt)
  ExpressionApplication a -> Internal.ExpressionApplication <$> goApplication a
  ExpressionCase a -> Internal.ExpressionCase <$> goCase a
  ExpressionInfixApplication ia -> Internal.ExpressionApplication <$> goInfix ia
  ExpressionPostfixApplication pa -> Internal.ExpressionApplication <$> goPostfix pa
  ExpressionLiteral l -> return (Internal.ExpressionLiteral (goLiteral l))
  ExpressionLambda l -> Internal.ExpressionLambda <$> goLambda l
  ExpressionBraces b -> throw (ErrAppLeftImplicit (AppLeftImplicit b))
  ExpressionLet l -> Internal.ExpressionLet <$> goLet l
  ExpressionUniverse uni -> return (Internal.ExpressionUniverse (goUniverse uni))
  ExpressionFunction func -> Internal.ExpressionFunction <$> goFunction func
  ExpressionHole h -> return (Internal.ExpressionHole h)
  ExpressionIterator i -> goIterator i
  where
    goIden :: Concrete.ScopedIden -> Internal.Expression
    goIden x = Internal.ExpressionIden $ case x of
      ScopedAxiom a -> Internal.IdenAxiom (goName (a ^. Concrete.axiomRefName))
      ScopedInductive i -> Internal.IdenInductive (goName (i ^. Concrete.inductiveRefName))
      ScopedVar v -> Internal.IdenVar (goSymbol v)
      ScopedFunction fun -> Internal.IdenFunction (goName (fun ^. Concrete.functionRefName))
      ScopedConstructor c -> Internal.IdenConstructor (goName (c ^. Concrete.constructorRefName))

    goLet :: Let 'Scoped -> Sem r Internal.Let
    goLet l = do
      _letExpression <- goExpression (l ^. letExpression)
      _letClauses <- goLetClauses (l ^. letClauses)
      return Internal.Let {..}
      where
        goLetClauses :: NonEmpty (LetClause 'Scoped) -> Sem r (NonEmpty Internal.LetClause)
        goLetClauses cl =
          nonEmpty' <$> sequence [Internal.LetFunDef <$> goSig sig | LetTypeSig sig <- toList cl]
          where
            goSig :: TypeSignature 'Scoped -> Sem r Internal.FunctionDef
            goSig sig = goLetFunctionDef sig getClauses
              where
                getClauses :: [FunctionClause 'Scoped]
                getClauses =
                  [ c | LetFunClause c <- toList cl, sig ^. sigName == c ^. clauseOwnerFunction
                  ]

    goApplication :: Application -> Sem r Internal.Application
    goApplication (Application l arg) = do
      l' <- goExpression l
      r' <- goExpression r
      return (Internal.Application l' r' i)
      where
        (r, i) = case arg of
          ExpressionBraces b -> (b ^. withLocParam, Implicit)
          _ -> (arg, Explicit)

    goPostfix :: PostfixApplication -> Sem r Internal.Application
    goPostfix (PostfixApplication l op) = do
      l' <- goExpression l
      let op' = goIden op
      return (Internal.Application op' l' Explicit)

    goInfix :: InfixApplication -> Sem r Internal.Application
    goInfix (InfixApplication l op r) = do
      l' <- goExpression l
      let op' = goIden op
          l'' = Internal.ExpressionApplication (Internal.Application op' l' Explicit)
      r' <- goExpression r
      return (Internal.Application l'' r' Explicit)

    goIterator :: Iterator 'Scoped -> Sem r Internal.Expression
    goIterator Iterator {..} = do
      inipats' <- mapM goPatternArg inipats
      rngpats' <- mapM goPatternArg rngpats
      expr <- goExpression _iteratorBody
      let lam =
            Internal.ExpressionLambda $
              Internal.Lambda
                { _lambdaClauses = Internal.LambdaClause (nonEmpty' (inipats' ++ rngpats')) expr :| [],
                  _lambdaType = Nothing
                }

          fn = goIden _iteratorName
      inivals' <- mapM goExpression inivals
      rngvals' <- mapM goExpression rngvals
      return $ foldl' mkApp fn (lam : inivals' ++ rngvals')
      where
        inipats = map (^. initializerPattern) _iteratorInitializers
        inivals = map (^. initializerExpression) _iteratorInitializers
        rngpats = map (^. rangePattern) _iteratorRanges
        rngvals = map (^. rangeExpression) _iteratorRanges

        mkApp :: Internal.Expression -> Internal.Expression -> Internal.Expression
        mkApp a1 a2 = Internal.ExpressionApplication $ Internal.Application a1 a2 Explicit

goCase :: forall r. Members '[NameIdGen, Error ScoperError, Reader Pragmas] r => Case 'Scoped -> Sem r Internal.Case
goCase c = do
  _caseExpression <- goExpression (c ^. caseExpression)
  _caseBranches <- mapM goBranch (c ^. caseBranches)
  let _caseParens = c ^. caseParens
      _caseExpressionType :: Maybe Internal.Expression = Nothing
      _caseExpressionWholeType :: Maybe Internal.Expression = Nothing
  return Internal.Case {..}
  where
    goBranch :: CaseBranch 'Scoped -> Sem r Internal.CaseBranch
    goBranch b = do
      _caseBranchPattern <- goPatternArg (b ^. caseBranchPattern)
      _caseBranchExpression <- goExpression (b ^. caseBranchExpression)
      return Internal.CaseBranch {..}

goLambda :: forall r. Members '[NameIdGen, Error ScoperError, Reader Pragmas] r => Lambda 'Scoped -> Sem r Internal.Lambda
goLambda l = do
  clauses' <- mapM goClause (l ^. lambdaClauses)
  return
    Internal.Lambda
      { _lambdaClauses = clauses',
        _lambdaType = Nothing
      }
  where
    goClause :: LambdaClause 'Scoped -> Sem r Internal.LambdaClause
    goClause lc = do
      ps' <- mapM goPatternArg (lc ^. lambdaParameters)
      b' <- goExpression (lc ^. lambdaBody)
      return (Internal.LambdaClause ps' b')

goUniverse :: Universe -> SmallUniverse
goUniverse u
  | isSmallUniverse u = SmallUniverse (getLoc u)
  | otherwise = error "only small universe is supported"

goFunction :: Members '[NameIdGen, Error ScoperError, Reader Pragmas] r => Function 'Scoped -> Sem r Internal.Function
goFunction f = do
  params <- goFunctionParameters (f ^. funParameters)
  ret <- goExpression (f ^. funReturn)
  return $
    Internal.Function (head params) $
      foldr (\param acc -> Internal.ExpressionFunction $ Internal.Function param acc) ret (NonEmpty.tail params)

goFunctionParameters ::
  Members '[NameIdGen, Error ScoperError, Reader Pragmas] r =>
  FunctionParameters 'Scoped ->
  Sem r (NonEmpty Internal.FunctionParameter)
goFunctionParameters FunctionParameters {..} = do
  _paramType' <- goExpression _paramType
  let mkParam param =
        Internal.FunctionParameter
          { Internal._paramType = _paramType',
            Internal._paramImplicit = _paramImplicit,
            Internal._paramName = goSymbol <$> param
          }
  return . fromMaybe (pure (mkParam Nothing)) . nonEmpty $
    mkParam . goFunctionParameter <$> _paramNames
  where
    goFunctionParameter :: FunctionParameter 'Scoped -> Maybe (SymbolType 'Scoped)
    goFunctionParameter = \case
      FunctionParameterName n -> Just n
      FunctionParameterWildcard {} -> Nothing

mkConstructorApp :: Internal.ConstrName -> [Internal.PatternArg] -> Internal.ConstructorApp
mkConstructorApp a b = Internal.ConstructorApp a b Nothing

goPatternApplication ::
  Members '[NameIdGen, Error ScoperError] r =>
  PatternApp ->
  Sem r Internal.ConstructorApp
goPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternApplication a)

goPatternConstructor ::
  Members '[NameIdGen, Error ScoperError] r =>
  ConstructorRef ->
  Sem r Internal.ConstructorApp
goPatternConstructor a = uncurry mkConstructorApp <$> viewApp (PatternConstructor a)

goInfixPatternApplication ::
  Members '[NameIdGen, Error ScoperError] r =>
  PatternInfixApp ->
  Sem r Internal.ConstructorApp
goInfixPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternInfixApplication a)

goPostfixPatternApplication ::
  Members '[NameIdGen, Error ScoperError] r =>
  PatternPostfixApp ->
  Sem r Internal.ConstructorApp
goPostfixPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternPostfixApplication a)

viewApp :: forall r. Members '[NameIdGen, Error ScoperError] r => Pattern -> Sem r (Internal.ConstrName, [Internal.PatternArg])
viewApp p = case p of
  PatternConstructor c -> return (goConstructorRef c, [])
  PatternApplication app@(PatternApp _ r) -> do
    r' <- goPatternArg r
    second (`snoc` r') <$> viewAppLeft app
  PatternInfixApplication (PatternInfixApp l c r) -> do
    l' <- goPatternArg l
    r' <- goPatternArg r
    return (goConstructorRef c, [l', r'])
  PatternPostfixApplication (PatternPostfixApp l c) -> do
    l' <- goPatternArg l
    return (goConstructorRef c, [l'])
  PatternVariable {} -> err
  PatternWildcard {} -> err
  PatternEmpty {} -> err
  where
    viewAppLeft :: PatternApp -> Sem r (Internal.ConstrName, [Internal.PatternArg])
    viewAppLeft app@(PatternApp l _)
      | Implicit <- l ^. patternArgIsImplicit = throw (ErrImplicitPatternLeftApplication (ImplicitPatternLeftApplication app))
      | otherwise = viewApp (l ^. patternArgPattern)
    err = throw (ErrConstructorExpectedLeftApplication (ConstructorExpectedLeftApplication p))

goConstructorRef :: ConstructorRef -> Internal.Name
goConstructorRef (ConstructorRef' n) = goName n

goPatternArg :: Members '[NameIdGen, Error ScoperError] r => PatternArg -> Sem r Internal.PatternArg
goPatternArg p = do
  pat' <- goPattern (p ^. patternArgPattern)
  return
    Internal.PatternArg
      { _patternArgIsImplicit = p ^. patternArgIsImplicit,
        _patternArgName = goSymbol <$> p ^. patternArgName,
        _patternArgPattern = pat'
      }

goPattern :: Members '[NameIdGen, Error ScoperError] r => Pattern -> Sem r Internal.Pattern
goPattern p = case p of
  PatternVariable a -> return $ Internal.PatternVariable (goSymbol a)
  PatternConstructor c -> Internal.PatternConstructorApp <$> goPatternConstructor c
  PatternApplication a -> Internal.PatternConstructorApp <$> goPatternApplication a
  PatternInfixApplication a -> Internal.PatternConstructorApp <$> goInfixPatternApplication a
  PatternPostfixApplication a -> Internal.PatternConstructorApp <$> goPostfixPatternApplication a
  PatternWildcard i -> Internal.PatternVariable <$> varFromWildcard i
  PatternEmpty {} -> error "unsupported empty pattern"

goAxiom :: (Members '[Reader Pragmas, Error ScoperError, Builtins, NameIdGen] r) => AxiomDef 'Scoped -> Sem r Internal.AxiomDef
goAxiom a = do
  _axiomType' <- goExpression (a ^. axiomType)
  _axiomPragmas' <- goPragmas (a ^. axiomPragmas)
  let axiom =
        Internal.AxiomDef
          { _axiomType = _axiomType',
            _axiomBuiltin = (^. withLocParam) <$> a ^. axiomBuiltin,
            _axiomName = goSymbol (a ^. axiomName),
            _axiomPragmas = _axiomPragmas'
          }
  whenJust (a ^. axiomBuiltin) (registerBuiltinAxiom axiom . (^. withLocParam))
  return axiom
