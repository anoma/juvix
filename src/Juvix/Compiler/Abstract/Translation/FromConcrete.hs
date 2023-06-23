module Juvix.Compiler.Abstract.Translation.FromConcrete
  ( module Juvix.Compiler.Abstract.Translation.FromConcrete,
    module Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Abstract.Data.InfoTableBuilder
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
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
unsupported msg = error $ msg <> "Scoped to Abstract: not yet supported"

fromConcrete ::
  Members '[Error JuvixError, Builtins, NameIdGen] r =>
  Scoper.ScoperResult ->
  Sem r AbstractResult
fromConcrete _resultScoper =
  mapError (JuvixError @ScoperError) $ do
    (_resultTable, (_resultModulesCache, _resultModules)) <-
      runInfoTableBuilder
        . runReader @Pragmas mempty
        . runState (ModulesCache mempty)
        . evalState iniState
        $ mapM goTopModule ms
    let _resultExports = _resultScoper ^. Scoper.resultExports
    return AbstractResult {..}
  where
    ms = _resultScoper ^. Scoper.resultModules

fromConcreteExpression :: (Members '[Error JuvixError, NameIdGen] r) => Scoper.Expression -> Sem r Abstract.Expression
fromConcreteExpression = mapError (JuvixError @ScoperError) . ignoreInfoTableBuilder . runReader @Pragmas mempty . goExpression

fromConcreteImport ::
  Members '[Error JuvixError, NameIdGen, Builtins, InfoTableBuilder, State ModulesCache, State TranslationState] r =>
  Scoper.Import 'Scoped ->
  Sem r (Maybe Abstract.Include)
fromConcreteImport =
  mapError (JuvixError @ScoperError)
    . runReader @Pragmas mempty
    . goImport

fromConcreteOpenImport ::
  Members '[Error JuvixError, NameIdGen, Builtins, InfoTableBuilder, State ModulesCache, State TranslationState] r =>
  Scoper.OpenModule 'Scoped ->
  Sem r (Maybe Abstract.Include)
fromConcreteOpenImport = mapError (JuvixError @ScoperError) . runReader @Pragmas mempty . goOpenModule'

-- | returns (cacheHit, module)
goTopModule ::
  forall r.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r) =>
  Module 'Scoped 'ModuleTop ->
  Sem r Abstract.TopModule
goTopModule m = do
  let moduleNameId :: S.NameId
      moduleNameId = m ^. Concrete.modulePath . S.nameId
      processModule :: Sem r Abstract.Module
      processModule = do
        am <- goModule' m
        modify (over cachedModules (HashMap.insert moduleNameId am))
        return am
  cache <- gets (^. cachedModules)
  maybe processModule return (cache ^. at moduleNameId)

goLocalModule ::
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r) =>
  Module 'Scoped 'ModuleLocal ->
  Sem r [Abstract.Statement]
goLocalModule = concatMapM goStatement . (^. moduleBody)

goModule' ::
  forall r t.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r, SingI t) =>
  Module 'Scoped t ->
  Sem r Abstract.Module
goModule' Module {..} = do
  pragmas' <- goPragmas _modulePragmas
  body' <- local (const pragmas') (goModuleBody _moduleBody)
  examples' <- goExamples _moduleDoc
  return
    Abstract.Module
      { _moduleName = name',
        _moduleBody = body',
        _moduleExamples = examples',
        _modulePragmas = pragmas'
      }
  where
    name' :: Abstract.Name
    name' = case sing :: SModuleIsTop t of
      SModuleTop -> goSymbol (S.topModulePathName _modulePath)
      SModuleLocal -> goSymbol _modulePath

goPragmas :: Member (Reader Pragmas) r => Maybe ParsedPragmas -> Sem r Pragmas
goPragmas p = do
  p' <- ask
  return $ p' <> p ^. _Just . withLocParam . withSourceValue

goName :: S.Name -> Abstract.Name
goName name =
  set Abstract.namePretty prettyStr (goSymbol (S.nameUnqualify name))
  where
    prettyStr :: Text
    prettyStr = prettyText name

goSymbol :: S.Symbol -> Abstract.Name
goSymbol s =
  Abstract.Name
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

goModuleBody ::
  forall r.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r) =>
  [Statement 'Scoped] ->
  Sem r Abstract.ModuleBody
goModuleBody ss' = do
  otherThanFunctions :: [Indexed Abstract.Statement] <- concatMapM (traverseM' goStatement) ss
  functions <- map (fmap Abstract.StatementFunction) <$> compiledFunctions
  let _moduleStatements =
        map
          (^. indexedThing)
          ( sortOn
              (^. indexedIx)
              (otherThanFunctions <> functions)
          )
  return Abstract.ModuleBody {..}
  where
    ss :: [Indexed (Statement 'Scoped)]
    ss = zipWith Indexed [0 ..] ss'

    compiledFunctions :: Sem r [Indexed Abstract.FunctionDef]
    compiledFunctions =
      sequence $
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

-- goImportCacheMiss ::
--   forall r.
--   (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache, Reader Pragmas] r) =>
--   Import 'Scoped ->
--   Sem r Abstract.TopModule
-- goImportCacheMiss t = snd <$> (goTopModule (t ^. importModule . moduleRefModule))

goImport ::
  forall r.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache, Reader Pragmas, State TranslationState] r) =>
  Import 'Scoped ->
  Sem r (Maybe Abstract.Include)
goImport Import {..} = do
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
                Abstract.Include
                  { _includeModule = m'
                  }
            )

guardNotCached :: (Bool, Abstract.TopModule) -> Maybe Abstract.TopModule
guardNotCached (hit, m) = do
  guard (not hit)
  return m

goStatement ::
  forall r.
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r =>
  Statement 'Scoped ->
  Sem r [Abstract.Statement]
goStatement = \case
  StatementAxiom d -> pure . Abstract.StatementAxiom <$> goAxiom d
  StatementImport t -> maybeToList . fmap Abstract.StatementInclude <$> goImport t
  StatementSyntax {} -> return []
  StatementOpenModule o -> maybeToList <$> goOpenModule o
  StatementInductive i -> pure . Abstract.StatementInductive <$> goInductive i
  StatementModule f -> goLocalModule f
  StatementTypeSignature {} -> return []
  StatementFunctionClause {} -> return []

goOpenModule' ::
  forall r.
  Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache, Reader Pragmas, State TranslationState] r =>
  OpenModule 'Scoped ->
  Sem r (Maybe Abstract.Include)
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
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ModulesCache, State TranslationState] r) =>
  OpenModule 'Scoped ->
  Sem r (Maybe Abstract.Statement)
goOpenModule o = fmap Abstract.StatementInclude <$> goOpenModule' o

goLetFunctionDef ::
  (Members '[InfoTableBuilder, Reader Pragmas, Error ScoperError] r) =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Abstract.FunctionDef
goLetFunctionDef = goFunctionDefHelper

goFunctionDefHelper ::
  forall r.
  (Members '[InfoTableBuilder, Reader Pragmas, Error ScoperError] r) =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Abstract.FunctionDef
goFunctionDefHelper sig@TypeSignature {..} clauses = do
  let _funDefName = goSymbol _sigName
      _funDefTerminating = isJust _sigTerminating
      _funDefBuiltin = (^. withLocParam) <$> _sigBuiltin
  _funDefTypeSig <- goExpression _sigType
  _funDefExamples <- goExamples _sigDoc
  _funDefPragmas <- goPragmas _sigPragmas
  _funDefClauses <- case (_sigBody, nonEmpty clauses) of
    (Nothing, Nothing) -> throw (ErrLacksFunctionClause (LacksFunctionClause sig))
    (Just {}, Just clauses') -> throw (ErrDuplicateFunctionClause (DuplicateFunctionClause sig (head clauses')))
    (Just body, Nothing) -> do
      body' <- goExpression body
      return
        ( pure
            Abstract.FunctionClause
              { _clauseName = _funDefName,
                _clausePatterns = [],
                _clauseBody = body'
              }
        )
    (Nothing, Just clauses') -> mapM goFunctionClause clauses'
  let fun = Abstract.FunctionDef {..}
  registerFunction' fun

goTopFunctionDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader Pragmas, Error ScoperError, Builtins, NameIdGen] r) =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Abstract.FunctionDef
goTopFunctionDef sig clauses = do
  fun <- goFunctionDefHelper sig clauses
  whenJust (sig ^. sigBuiltin) (registerBuiltinFunction fun . (^. withLocParam))
  return fun

goExamples ::
  forall r.
  (Members '[Error ScoperError, Reader Pragmas, InfoTableBuilder] r) =>
  Maybe (Judoc 'Scoped) ->
  Sem r [Abstract.Example]
goExamples = mapM goExample . maybe [] judocExamples
  where
    goExample :: Example 'Scoped -> Sem r Abstract.Example
    goExample ex = do
      e' <- goExpression (ex ^. exampleExpression)
      return
        Abstract.Example
          { _exampleExpression = e',
            _exampleId = ex ^. exampleId
          }

goFunctionClause ::
  (Members '[Error ScoperError, Reader Pragmas, InfoTableBuilder] r) =>
  FunctionClause 'Scoped ->
  Sem r Abstract.FunctionClause
goFunctionClause FunctionClause {..} = do
  _clausePatterns' <- mapM goPatternArg _clausePatterns
  _clauseBody' <- goExpression _clauseBody
  return
    Abstract.FunctionClause
      { _clauseName = goSymbol _clauseOwnerFunction,
        _clausePatterns = _clausePatterns',
        _clauseBody = _clauseBody'
      }

goInductiveParameters ::
  Members '[Error ScoperError, Reader Pragmas, InfoTableBuilder] r =>
  InductiveParameters 'Scoped ->
  Sem r [Abstract.InductiveParameter]
goInductiveParameters InductiveParameters {..} = do
  paramType' <- goExpression _inductiveParametersType
  case paramType' of
    Abstract.ExpressionUniverse {} -> return ()
    _ -> unsupported "only type variables of small types are allowed"

  let goInductiveParameter :: S.Symbol -> Abstract.InductiveParameter
      goInductiveParameter var =
        Abstract.InductiveParameter
          { _inductiveParamName = goSymbol var
          }
  return (map goInductiveParameter (toList _inductiveParametersNames))

registerBuiltinInductive ::
  (Members '[InfoTableBuilder, Error ScoperError, Builtins] r) =>
  Abstract.InductiveDef ->
  BuiltinInductive ->
  Sem r ()
registerBuiltinInductive d = \case
  BuiltinNat -> registerNatDef d
  BuiltinBool -> registerBoolDef d
  BuiltinInt -> registerIntDef d

registerBuiltinFunction ::
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen] r) =>
  Abstract.FunctionDef ->
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
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen] r) =>
  Abstract.AxiomDef ->
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
  (Members '[InfoTableBuilder, Reader Pragmas, Builtins, Error ScoperError] r) =>
  InductiveDef 'Scoped ->
  Sem r Abstract.InductiveDef
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
        Abstract.InductiveDef
          { _inductiveParameters = _inductiveParameters',
            _inductiveBuiltin = (^. withLocParam) <$> _inductiveBuiltin,
            _inductiveName = goSymbol _inductiveName,
            _inductiveType = fromMaybe (Abstract.ExpressionUniverse (SmallUniverse loc)) _inductiveType',
            _inductiveConstructors = toList _inductiveConstructors',
            _inductiveExamples = _inductiveExamples',
            _inductivePragmas = _inductivePragmas',
            _inductivePositive = isJust (ty ^. inductivePositive)
          }
  whenJust ((^. withLocParam) <$> _inductiveBuiltin) (registerBuiltinInductive indDef)
  inductiveInfo <- registerInductive indDef
  forM_ _inductiveConstructors' (registerConstructor inductiveInfo)
  return (inductiveInfo ^. inductiveInfoDef)

goConstructorDef ::
  (Members [Error ScoperError, Reader Pragmas, InfoTableBuilder] r) =>
  InductiveConstructorDef 'Scoped ->
  Sem r Abstract.InductiveConstructorDef
goConstructorDef InductiveConstructorDef {..} = do
  ty' <- goExpression _constructorType
  examples' <- goExamples _constructorDoc
  pragmas' <- goPragmas _constructorPragmas
  return
    Abstract.InductiveConstructorDef
      { _constructorType = ty',
        _constructorExamples = examples',
        _constructorName = goSymbol _constructorName,
        _constructorPragmas = pragmas'
      }

goExpression ::
  forall r.
  (Members [Error ScoperError, Reader Pragmas, InfoTableBuilder] r) =>
  Expression ->
  Sem r Abstract.Expression
goExpression = \case
  ExpressionIdentifier nt -> return (goIden nt)
  ExpressionParensIdentifier nt -> return (goIden nt)
  ExpressionApplication a -> Abstract.ExpressionApplication <$> goApplication a
  ExpressionCase a -> Abstract.ExpressionCase <$> goCase a
  ExpressionInfixApplication ia -> Abstract.ExpressionApplication <$> goInfix ia
  ExpressionPostfixApplication pa -> Abstract.ExpressionApplication <$> goPostfix pa
  ExpressionLiteral l -> return (Abstract.ExpressionLiteral l)
  ExpressionLambda l -> Abstract.ExpressionLambda <$> goLambda l
  ExpressionBraces b -> throw (ErrAppLeftImplicit (AppLeftImplicit b))
  ExpressionLet l -> Abstract.ExpressionLet <$> goLet l
  ExpressionUniverse uni -> return (Abstract.ExpressionUniverse (goUniverse uni))
  ExpressionFunction func -> Abstract.ExpressionFunction <$> goFunction func
  ExpressionHole h -> return (Abstract.ExpressionHole h)
  ExpressionIterator i -> goIterator i
  where
    goIden :: Concrete.ScopedIden -> Abstract.Expression
    goIden x = Abstract.ExpressionIden $ case x of
      ScopedAxiom a -> Abstract.IdenAxiom (goName (a ^. Concrete.axiomRefName))
      ScopedInductive i -> Abstract.IdenInductive (goName (i ^. Concrete.inductiveRefName))
      ScopedVar v -> Abstract.IdenVar (goSymbol v)
      ScopedFunction fun -> Abstract.IdenFunction (goName (fun ^. Concrete.functionRefName))
      ScopedConstructor c -> Abstract.IdenConstructor (goName (c ^. Concrete.constructorRefName))

    goLet :: Let 'Scoped -> Sem r Abstract.Let
    goLet l = do
      _letExpression <- goExpression (l ^. letExpression)
      _letClauses <- goLetClauses (l ^. letClauses)
      return Abstract.Let {..}
      where
        goLetClauses :: NonEmpty (LetClause 'Scoped) -> Sem r (NonEmpty Abstract.LetClause)
        goLetClauses cl =
          nonEmpty' <$> sequence [Abstract.LetFunDef <$> goSig sig | LetTypeSig sig <- toList cl]
          where
            goSig :: TypeSignature 'Scoped -> Sem r Abstract.FunctionDef
            goSig sig = goLetFunctionDef sig getClauses
              where
                getClauses :: [FunctionClause 'Scoped]
                getClauses =
                  [ c | LetFunClause c <- toList cl, sig ^. sigName == c ^. clauseOwnerFunction
                  ]

    goApplication :: Application -> Sem r Abstract.Application
    goApplication (Application l arg) = do
      l' <- goExpression l
      r' <- goExpression r
      return (Abstract.Application l' r' i)
      where
        (r, i) = case arg of
          ExpressionBraces b -> (b ^. withLocParam, Implicit)
          _ -> (arg, Explicit)

    goPostfix :: PostfixApplication -> Sem r Abstract.Application
    goPostfix (PostfixApplication l op) = do
      l' <- goExpression l
      let op' = goIden op
      return (Abstract.Application op' l' Explicit)

    goInfix :: InfixApplication -> Sem r Abstract.Application
    goInfix (InfixApplication l op r) = do
      l' <- goExpression l
      let op' = goIden op
          l'' = Abstract.ExpressionApplication (Abstract.Application op' l' Explicit)
      r' <- goExpression r
      return (Abstract.Application l'' r' Explicit)

    goIterator :: Iterator 'Scoped -> Sem r Abstract.Expression
    goIterator Iterator {..} = do
      inipats' <- mapM goPatternArg inipats
      rngpats' <- mapM goPatternArg rngpats
      expr <- goExpression _iteratorBody
      let lam =
            Abstract.ExpressionLambda $
              Abstract.Lambda $
                Abstract.LambdaClause (nonEmpty' (inipats' ++ rngpats')) expr :| []
          fn = goIden _iteratorName
      inivals' <- mapM goExpression inivals
      rngvals' <- mapM goExpression rngvals
      return $ foldl' mkApp fn (lam : inivals' ++ rngvals')
      where
        inipats = map (^. initializerPattern) _iteratorInitializers
        inivals = map (^. initializerExpression) _iteratorInitializers
        rngpats = map (^. rangePattern) _iteratorRanges
        rngvals = map (^. rangeExpression) _iteratorRanges

        mkApp :: Abstract.Expression -> Abstract.Expression -> Abstract.Expression
        mkApp a1 a2 = Abstract.ExpressionApplication $ Abstract.Application a1 a2 Explicit

goCase :: forall r. (Members '[Error ScoperError, Reader Pragmas, InfoTableBuilder] r) => Case 'Scoped -> Sem r Abstract.Case
goCase c = do
  _caseExpression <- goExpression (c ^. caseExpression)
  _caseBranches <- mapM goBranch (c ^. caseBranches)
  let _caseParens = c ^. caseParens
  return Abstract.Case {..}
  where
    goBranch :: CaseBranch 'Scoped -> Sem r Abstract.CaseBranch
    goBranch b = do
      _caseBranchPattern <- goPatternArg (b ^. caseBranchPattern)
      _caseBranchExpression <- goExpression (b ^. caseBranchExpression)
      return Abstract.CaseBranch {..}

goLambda :: forall r. (Members '[Error ScoperError, Reader Pragmas, InfoTableBuilder] r) => Lambda 'Scoped -> Sem r Abstract.Lambda
goLambda l = Abstract.Lambda <$> mapM goClause (l ^. lambdaClauses)
  where
    goClause :: LambdaClause 'Scoped -> Sem r Abstract.LambdaClause
    goClause lc = do
      ps' <- mapM goPatternArg (lc ^. lambdaParameters)
      b' <- goExpression (lc ^. lambdaBody)
      return (Abstract.LambdaClause ps' b')

goUniverse :: Universe -> SmallUniverse
goUniverse u
  | isSmallUniverse u = SmallUniverse (getLoc u)
  | otherwise = error "only small universe is supported"

goFunction :: (Members '[Error ScoperError, Reader Pragmas, InfoTableBuilder] r) => Function 'Scoped -> Sem r Abstract.Function
goFunction f = do
  params <- goFunctionParameters (f ^. funParameters)
  ret <- goExpression (f ^. funReturn)
  return $
    Abstract.Function (head params) $
      foldr (\param acc -> Abstract.ExpressionFunction $ Abstract.Function param acc) ret (NonEmpty.tail params)

goFunctionParameters ::
  (Members '[Error ScoperError, Reader Pragmas, InfoTableBuilder] r) =>
  FunctionParameters 'Scoped ->
  Sem r (NonEmpty Abstract.FunctionParameter)
goFunctionParameters FunctionParameters {..} = do
  _paramType' <- goExpression _paramType
  let mkParam param =
        Abstract.FunctionParameter
          { Abstract._paramType = _paramType',
            Abstract._paramImplicit = _paramImplicit,
            Abstract._paramName = goSymbol <$> param
          }
  return . fromMaybe (pure (mkParam Nothing)) . nonEmpty $
    mkParam . goFunctionParameter <$> _paramNames
  where
    goFunctionParameter :: FunctionParameter 'Scoped -> Maybe (SymbolType 'Scoped)
    goFunctionParameter = \case
      FunctionParameterName n -> Just n
      FunctionParameterWildcard {} -> Nothing

mkConstructorApp :: Abstract.ConstrName -> [Abstract.PatternArg] -> Abstract.ConstructorApp
mkConstructorApp a b = Abstract.ConstructorApp a b Nothing

goPatternApplication ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  PatternApp ->
  Sem r Abstract.ConstructorApp
goPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternApplication a)

goPatternConstructor ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  ConstructorRef ->
  Sem r Abstract.ConstructorApp
goPatternConstructor a = uncurry mkConstructorApp <$> viewApp (PatternConstructor a)

goInfixPatternApplication ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  PatternInfixApp ->
  Sem r Abstract.ConstructorApp
goInfixPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternInfixApplication a)

goPostfixPatternApplication ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  PatternPostfixApp ->
  Sem r Abstract.ConstructorApp
goPostfixPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternPostfixApplication a)

viewApp :: forall r. (Members '[Error ScoperError, InfoTableBuilder] r) => Pattern -> Sem r (Abstract.ConstrName, [Abstract.PatternArg])
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
    viewAppLeft :: PatternApp -> Sem r (Abstract.ConstrName, [Abstract.PatternArg])
    viewAppLeft app@(PatternApp l _)
      | Implicit <- l ^. patternArgIsImplicit = throw (ErrImplicitPatternLeftApplication (ImplicitPatternLeftApplication app))
      | otherwise = viewApp (l ^. patternArgPattern)
    err = throw (ErrConstructorExpectedLeftApplication (ConstructorExpectedLeftApplication p))

goConstructorRef :: ConstructorRef -> Abstract.Name
goConstructorRef (ConstructorRef' n) = goName n

goPatternArg :: (Members '[Error ScoperError, InfoTableBuilder] r) => PatternArg -> Sem r Abstract.PatternArg
goPatternArg p = do
  pat' <- goPattern (p ^. patternArgPattern)
  return
    Abstract.PatternArg
      { _patternArgIsImplicit = p ^. patternArgIsImplicit,
        _patternArgName = goSymbol <$> p ^. patternArgName,
        _patternArgPattern = pat'
      }

goPattern :: (Members '[Error ScoperError, InfoTableBuilder] r) => Pattern -> Sem r Abstract.Pattern
goPattern p = case p of
  PatternVariable a -> return $ Abstract.PatternVariable (goSymbol a)
  PatternConstructor c -> Abstract.PatternConstructorApp <$> goPatternConstructor c
  PatternApplication a -> Abstract.PatternConstructorApp <$> goPatternApplication a
  PatternInfixApplication a -> Abstract.PatternConstructorApp <$> goInfixPatternApplication a
  PatternPostfixApplication a -> Abstract.PatternConstructorApp <$> goPostfixPatternApplication a
  PatternWildcard i -> return (Abstract.PatternWildcard i)
  PatternEmpty {} -> error "unsupported empty pattern"

goAxiom :: (Members '[InfoTableBuilder, Reader Pragmas, Error ScoperError, Builtins, NameIdGen] r) => AxiomDef 'Scoped -> Sem r Abstract.AxiomDef
goAxiom a = do
  _axiomType' <- goExpression (a ^. axiomType)
  _axiomPragmas' <- goPragmas (a ^. axiomPragmas)
  let axiom =
        Abstract.AxiomDef
          { _axiomType = _axiomType',
            _axiomBuiltin = (^. withLocParam) <$> a ^. axiomBuiltin,
            _axiomName = goSymbol (a ^. axiomName),
            _axiomPragmas = _axiomPragmas'
          }
  whenJust (a ^. axiomBuiltin) (registerBuiltinAxiom axiom . (^. withLocParam))
  registerAxiom' axiom
