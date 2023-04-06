module Juvix.Compiler.Abstract.Translation.FromConcrete
  ( module Juvix.Compiler.Abstract.Translation.FromConcrete,
    module Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Abstract.Data.InfoTableBuilder
import Juvix.Compiler.Abstract.Language (FunctionDef (_funDefExamples))
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Data.NameKind
import Juvix.Prelude

newtype ModulesCache = ModulesCache
  {_cachedModules :: HashMap S.NameId Abstract.TopModule}

makeLenses ''ModulesCache

unsupported :: Text -> a
unsupported msg = error $ msg <> "Scoped to Abstract: not yet supported"

fromConcrete :: (Members '[Error JuvixError, Builtins, NameIdGen] r) => Scoper.ScoperResult -> Sem r AbstractResult
fromConcrete _resultScoper =
  mapError (JuvixError @ScoperError) $ do
    (_resultTable, _resultModules) <- runInfoTableBuilder (evalState (ModulesCache mempty) (mapM goTopModule ms))
    let _resultExports = _resultScoper ^. Scoper.resultExports
    return AbstractResult {..}
  where
    ms = _resultScoper ^. Scoper.resultModules

fromConcreteExpression :: (Members '[Error JuvixError, NameIdGen] r) => Scoper.Expression -> Sem r Abstract.Expression
fromConcreteExpression = mapError (JuvixError @ScoperError) . ignoreInfoTableBuilder . goExpression

goTopModule ::
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r) =>
  Module 'Scoped 'ModuleTop ->
  Sem r Abstract.TopModule
goTopModule = goModule

goLocalModule ::
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r) =>
  Module 'Scoped 'ModuleLocal ->
  Sem r Abstract.LocalModule
goLocalModule = goModule

goModule ::
  forall r t.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r, SingI t) =>
  Module 'Scoped t ->
  Sem r Abstract.Module
goModule m = case sing :: SModuleIsTop t of
  SModuleTop -> do
    cache <- gets (^. cachedModules)
    let moduleNameId :: S.NameId
        moduleNameId = m ^. Concrete.modulePath . S.nameId
    let processModule :: Sem r Abstract.Module
        processModule = do
          am <- goModule' m
          modify (over cachedModules (HashMap.insert moduleNameId am))
          return am
    maybe processModule return (cache ^. at moduleNameId)
  SModuleLocal -> goModule' m
  where
    goModule' :: Module 'Scoped t -> Sem r Abstract.Module
    goModule' Module {..} = do
      body' <- goModuleBody _moduleBody
      examples' <- goExamples _moduleDoc
      return
        Abstract.Module
          { _moduleName = name',
            _moduleBody = body',
            _moduleExamples = examples'
          }
      where
        name' :: Abstract.Name
        name' = case sing :: SModuleIsTop t of
          SModuleTop -> goSymbol (S.topModulePathName _modulePath)
          SModuleLocal -> goSymbol _modulePath

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

goModuleBody ::
  forall r.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r) =>
  [Statement 'Scoped] ->
  Sem r Abstract.ModuleBody
goModuleBody ss' = do
  otherThanFunctions <- mapMaybeM goStatement ss
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

goStatement ::
  forall r.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r) =>
  Indexed (Statement 'Scoped) ->
  Sem r (Maybe (Indexed Abstract.Statement))
goStatement (Indexed idx s) =
  fmap (Indexed idx) <$> case s of
    StatementAxiom d -> Just . Abstract.StatementAxiom <$> goAxiom d
    StatementImport t -> Just . Abstract.StatementImport <$> goModule (t ^. importModule . moduleRefModule)
    StatementOperator {} -> return Nothing
    StatementOpenModule o -> goOpenModule o
    StatementInductive i -> Just . Abstract.StatementInductive <$> goInductive i
    StatementModule f -> Just . Abstract.StatementLocalModule <$> goLocalModule f
    StatementTypeSignature {} -> return Nothing
    StatementFunctionClause {} -> return Nothing

goOpenModule ::
  forall r.
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen, State ModulesCache] r) =>
  OpenModule 'Scoped ->
  Sem r (Maybe Abstract.Statement)
goOpenModule o
  | isJust (o ^. openModuleImportKw) =
      case o ^. openModuleName of
        ModuleRef' (SModuleTop :&: m) ->
          Just . Abstract.StatementImport
            <$> goModule (m ^. moduleRefModule)
        _ -> impossible
  | otherwise = return Nothing

goLetFunctionDef ::
  (Members '[InfoTableBuilder, Error ScoperError] r) =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Abstract.FunctionDef
goLetFunctionDef = goFunctionDefHelper

goFunctionDefHelper ::
  forall r.
  (Members '[InfoTableBuilder, Error ScoperError] r) =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Abstract.FunctionDef
goFunctionDefHelper sig@TypeSignature {..} clauses = do
  let _funDefName = goSymbol _sigName
      _funDefTerminating = isJust _sigTerminating
      _funDefBuiltin = (^. withLocParam) <$> _sigBuiltin
  _funDefTypeSig <- goExpression _sigType
  _funDefExamples <- goExamples _sigDoc
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
  (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen] r) =>
  TypeSignature 'Scoped ->
  [FunctionClause 'Scoped] ->
  Sem r Abstract.FunctionDef
goTopFunctionDef sig clauses = do
  fun <- goFunctionDefHelper sig clauses
  whenJust (sig ^. sigBuiltin) (registerBuiltinFunction fun . (^. withLocParam))
  return fun

goExamples ::
  forall r.
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
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
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
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
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  InductiveParameters 'Scoped ->
  Sem r [Abstract.FunctionParameter]
goInductiveParameters InductiveParameters {..} = do
  paramType' <- goExpression _inductiveParametersType
  return $
    map
      ( \nm ->
          Abstract.FunctionParameter
            { _paramType = paramType',
              _paramName = Just (goSymbol nm),
              _paramImplicit = Explicit
            }
      )
      (toList _inductiveParametersNames)

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

goInductive ::
  (Members '[InfoTableBuilder, Builtins, Error ScoperError] r) =>
  InductiveDef 'Scoped ->
  Sem r Abstract.InductiveDef
goInductive ty@InductiveDef {..} = do
  _inductiveParameters' <- concatMapM goInductiveParameters _inductiveParameters
  _inductiveType' <- mapM goExpression _inductiveType
  _inductiveConstructors' <- mapM goConstructorDef _inductiveConstructors
  _inductiveExamples' <- goExamples _inductiveDoc
  let loc = getLoc _inductiveName
      indDef =
        Abstract.InductiveDef
          { _inductiveParameters = _inductiveParameters',
            _inductiveBuiltin = (^. withLocParam) <$> _inductiveBuiltin,
            _inductiveName = goSymbol _inductiveName,
            _inductiveType = fromMaybe (Abstract.ExpressionUniverse (smallUniverse loc)) _inductiveType',
            _inductiveConstructors = toList _inductiveConstructors',
            _inductiveExamples = _inductiveExamples',
            _inductivePositive = ty ^. inductivePositive
          }
  whenJust ((^. withLocParam) <$> _inductiveBuiltin) (registerBuiltinInductive indDef)
  inductiveInfo <- registerInductive indDef
  forM_ _inductiveConstructors' (registerConstructor inductiveInfo)
  return (inductiveInfo ^. inductiveInfoDef)

goConstructorDef ::
  (Members [Error ScoperError, InfoTableBuilder] r) =>
  InductiveConstructorDef 'Scoped ->
  Sem r Abstract.InductiveConstructorDef
goConstructorDef InductiveConstructorDef {..} = do
  ty' <- goExpression _constructorType
  examples' <- goExamples _constructorDoc
  return
    Abstract.InductiveConstructorDef
      { _constructorType = ty',
        _constructorExamples = examples',
        _constructorName = goSymbol _constructorName
      }

goExpression ::
  forall r.
  (Members [Error ScoperError, InfoTableBuilder] r) =>
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
  ExpressionLetBlock l -> Abstract.ExpressionLet <$> goLet l
  ExpressionUniverse uni -> return (Abstract.ExpressionUniverse (goUniverse uni))
  ExpressionFunction func -> Abstract.ExpressionFunction <$> goFunction func
  ExpressionHole h -> return (Abstract.ExpressionHole h)
  where
    goIden :: Concrete.ScopedIden -> Abstract.Expression
    goIden x = Abstract.ExpressionIden $ case x of
      ScopedAxiom a -> Abstract.IdenAxiom (Abstract.AxiomRef (goName (a ^. Concrete.axiomRefName)))
      ScopedInductive i -> Abstract.IdenInductive (Abstract.InductiveRef (goName (i ^. Concrete.inductiveRefName)))
      ScopedVar v -> Abstract.IdenVar (goSymbol v)
      ScopedFunction fun -> Abstract.IdenFunction (Abstract.FunctionRef (goName (fun ^. Concrete.functionRefName)))
      ScopedConstructor c -> Abstract.IdenConstructor (Abstract.ConstructorRef (goName (c ^. Concrete.constructorRefName)))

    goLet :: LetBlock 'Scoped -> Sem r Abstract.Let
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

goCase :: forall r. (Members '[Error ScoperError, InfoTableBuilder] r) => Case 'Scoped -> Sem r Abstract.Case
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

goLambda :: forall r. (Members '[Error ScoperError, InfoTableBuilder] r) => Lambda 'Scoped -> Sem r Abstract.Lambda
goLambda l = Abstract.Lambda <$> mapM goClause (l ^. lambdaClauses)
  where
    goClause :: LambdaClause 'Scoped -> Sem r Abstract.LambdaClause
    goClause lc = do
      ps' <- mapM goPatternArg (lc ^. lambdaParameters)
      b' <- goExpression (lc ^. lambdaBody)
      return (Abstract.LambdaClause ps' b')

goUniverse :: Universe -> Universe
goUniverse = id

goFunction :: (Members '[Error ScoperError, InfoTableBuilder] r) => Function 'Scoped -> Sem r Abstract.Function
goFunction f = do
  params <- goFunctionParameters (f ^. funParameters)
  ret <- goExpression (f ^. funReturn)
  return $
    Abstract.Function (head params) $
      foldr (\param acc -> Abstract.ExpressionFunction $ Abstract.Function param acc) ret (NonEmpty.tail params)

goFunctionParameters ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  FunctionParameters 'Scoped ->
  Sem r (NonEmpty Abstract.FunctionParameter)
goFunctionParameters (FunctionParameters {..}) = do
  _paramType' <- goExpression _paramType
  return $
    fmap
      ( \param ->
          Abstract.FunctionParameter
            { Abstract._paramType = _paramType',
              Abstract._paramImplicit = _paramImplicit,
              Abstract._paramName = goSymbol <$> param
            }
      )
      (fromMaybe (pure Nothing) (nonEmpty _paramNames))

goPatternApplication ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  PatternApp ->
  Sem r Abstract.ConstructorApp
goPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternApplication a)

goPatternConstructor ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  ConstructorRef ->
  Sem r Abstract.ConstructorApp
goPatternConstructor a = uncurry Abstract.ConstructorApp <$> viewApp (PatternConstructor a)

goInfixPatternApplication ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  PatternInfixApp ->
  Sem r Abstract.ConstructorApp
goInfixPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternInfixApplication a)

goPostfixPatternApplication ::
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  PatternPostfixApp ->
  Sem r Abstract.ConstructorApp
goPostfixPatternApplication a = uncurry Abstract.ConstructorApp <$> viewApp (PatternPostfixApplication a)

viewApp :: forall r. (Members '[Error ScoperError, InfoTableBuilder] r) => Pattern -> Sem r (Abstract.ConstructorRef, [Abstract.PatternArg])
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
    viewAppLeft :: PatternApp -> Sem r (Abstract.ConstructorRef, [Abstract.PatternArg])
    viewAppLeft app@(PatternApp l _)
      | Implicit <- l ^. patternArgIsImplicit = throw (ErrImplicitPatternLeftApplication (ImplicitPatternLeftApplication app))
      | otherwise = viewApp (l ^. patternArgPattern)
    err = throw (ErrConstructorExpectedLeftApplication (ConstructorExpectedLeftApplication p))

goConstructorRef :: ConstructorRef -> Abstract.ConstructorRef
goConstructorRef (ConstructorRef' n) = Abstract.ConstructorRef (goName n)

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
  PatternEmpty {} -> return Abstract.PatternEmpty

goAxiom :: (Members '[InfoTableBuilder, Error ScoperError, Builtins, NameIdGen] r) => AxiomDef 'Scoped -> Sem r Abstract.AxiomDef
goAxiom a = do
  _axiomType' <- goExpression (a ^. axiomType)
  let axiom =
        Abstract.AxiomDef
          { _axiomType = _axiomType',
            _axiomBuiltin = (^. withLocParam) <$> a ^. axiomBuiltin,
            _axiomName = goSymbol (a ^. axiomName)
          }
  whenJust (a ^. axiomBuiltin) (registerBuiltinAxiom axiom . (^. withLocParam))
  registerAxiom' axiom
