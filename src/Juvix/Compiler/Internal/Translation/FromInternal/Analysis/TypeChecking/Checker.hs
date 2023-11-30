module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Checker
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Checker,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Internal.Data.Cast
import Juvix.Compiler.Internal.Data.CoercionInfo
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Data.TypedHole
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (Termination)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Resolver
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Termination
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

type MCache = Cache ModuleIndex Module

registerConstructor :: (Members '[HighlightBuilder, State TypesTable, Reader InfoTable] r) => ConstructorDef -> Sem r ()
registerConstructor ctr = do
  ty <- lookupConstructorType (ctr ^. inductiveConstructorName)
  registerNameIdType (ctr ^. inductiveConstructorName . nameId) ty

registerNameIdType :: (Members '[HighlightBuilder, State TypesTable, Reader InfoTable] r) => NameId -> Expression -> Sem r ()
registerNameIdType uid ty = do
  modify (HashMap.insert uid ty)
  modify (set (highlightTypes . at uid) (Just ty))

checkTable ::
  (Members '[Reader InfoTable, Error TypeCheckerError] r) =>
  Sem r ()
checkTable = do
  tab <- ask
  let s = toList $ cyclicCoercions (tab ^. infoCoercions)
  whenJust (nonEmpty s) $
    throw
      . ErrCoercionCycles
      . CoercionCycles

checkModule ::
  (Members '[HighlightBuilder, Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, MCache] r) =>
  Module ->
  Sem r Module
checkModule = cacheGet . ModuleIndex

checkModuleIndex ::
  (Members '[HighlightBuilder, Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, MCache] r) =>
  ModuleIndex ->
  Sem r ModuleIndex
checkModuleIndex = fmap ModuleIndex . cacheGet

checkModuleNoCache ::
  (Members '[HighlightBuilder, Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, MCache, Termination] r) =>
  ModuleIndex ->
  Sem r Module
checkModuleNoCache (ModuleIndex Module {..}) = do
  _moduleBody' <-
    evalState (mempty :: NegativeTypeParameters)
      . checkModuleBody
      $ _moduleBody
  _moduleExamples <- mapM checkExample _moduleExamples
  return
    Module
      { _moduleBody = _moduleBody',
        _moduleName,
        _moduleExamples,
        _modulePragmas
      }

checkModuleBody ::
  (Members '[HighlightBuilder, Reader EntryPoint, State NegativeTypeParameters, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, MCache, Termination] r) =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleImports' <- mapM checkImport _moduleImports
  _moduleStatements' <- mapM checkMutualBlock _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements',
        _moduleImports = _moduleImports'
      }

checkImport ::
  (Members '[HighlightBuilder, Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, MCache, Termination] r) =>
  Import ->
  Sem r Import
checkImport = traverseOf importModule checkModuleIndex

checkMutualBlock ::
  (Members '[HighlightBuilder, Reader EntryPoint, State NegativeTypeParameters, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, Termination] r) =>
  MutualBlock ->
  Sem r MutualBlock
checkMutualBlock s = runReader emptyLocalVars (checkTopMutualBlock s)

checkInductiveDef ::
  forall r.
  (Members '[HighlightBuilder, Reader EntryPoint, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, NameIdGen, State TypesTable, State NegativeTypeParameters, Output Example, Builtins, Termination, Output TypedHole, Output CastHole] r) =>
  InductiveDef ->
  Sem r InductiveDef
checkInductiveDef InductiveDef {..} = runInferenceDef $ do
  constrs' <- mapM goConstructor _inductiveConstructors
  ty <- lookupInductiveType _inductiveName
  registerNameIdType (_inductiveName ^. nameId) ty
  examples' <- mapM checkExample _inductiveExamples
  inductiveType' <- runReader paramLocals (checkDefType _inductiveType)
  let d =
        InductiveDef
          { _inductiveConstructors = constrs',
            _inductiveExamples = examples',
            _inductiveType = inductiveType',
            _inductiveName,
            _inductiveBuiltin,
            _inductivePositive,
            _inductiveParameters,
            _inductiveTrait,
            _inductivePragmas
          }
  checkPositivity d
  return d
  where
    paramLocals :: LocalVars
    paramLocals =
      LocalVars
        { _localTypes = HashMap.fromList [(p ^. inductiveParamName, p ^. inductiveParamType) | p <- _inductiveParameters],
          _localTyMap = mempty
        }
    goConstructor :: ConstructorDef -> Sem (Inference ': r) ConstructorDef
    goConstructor ConstructorDef {..} = do
      expectedRetTy <- lookupConstructorReturnType _inductiveConstructorName
      cty' <-
        runReader paramLocals $
          checkIsType (getLoc _inductiveConstructorType) _inductiveConstructorType
      examples' <- mapM checkExample _inductiveConstructorExamples
      whenJustM (matchTypes expectedRetTy ret) (const (errRet expectedRetTy))
      let c' =
            ConstructorDef
              { _inductiveConstructorType = cty',
                _inductiveConstructorExamples = examples',
                _inductiveConstructorName,
                _inductiveConstructorPragmas
              }
      registerConstructor c'
      return c'
      where
        ret = snd (viewConstructorType _inductiveConstructorType)
        errRet :: Expression -> Sem (Inference ': r) a
        errRet expected =
          throw
            ( ErrWrongReturnType
                WrongReturnType
                  { _wrongReturnTypeConstructorName = _inductiveConstructorName,
                    _wrongReturnTypeExpected = expected,
                    _wrongReturnTypeActual = ret
                  }
            )

withEmptyVars :: Sem (Reader LocalVars ': r) a -> Sem r a
withEmptyVars = runReader emptyLocalVars

-- TODO should we register functions (type synonyms) first?
checkTopMutualBlock ::
  (Members '[HighlightBuilder, State NegativeTypeParameters, Reader EntryPoint, Reader LocalVars, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, Termination] r) =>
  MutualBlock ->
  Sem r MutualBlock
checkTopMutualBlock (MutualBlock ds) =
  MutualBlock <$> runInferenceDefs (mapM checkMutualStatement ds)

checkMutualStatement ::
  (Members '[HighlightBuilder, State NegativeTypeParameters, Reader EntryPoint, Inference, Reader LocalVars, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, Termination] r) =>
  MutualStatement ->
  Sem r MutualStatement
checkMutualStatement = \case
  StatementFunction f -> StatementFunction <$> resolveInstanceHoles (resolveCastHoles (checkFunctionDef f))
  StatementInductive f -> StatementInductive <$> resolveInstanceHoles (resolveCastHoles (checkInductiveDef f))
  StatementAxiom ax -> do
    registerNameIdType (ax ^. axiomName . nameId) (ax ^. axiomType)
    return $ StatementAxiom ax

checkFunctionDef ::
  forall r.
  (Members '[HighlightBuilder, Reader LocalVars, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, Inference, Termination, Output TypedHole, Output CastHole] r) =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  funDef <- do
    _funDefType' <- checkDefType _funDefType
    _funDefExamples' <- mapM checkExample _funDefExamples
    registerIdenType _funDefName _funDefType'
    _funDefBody' <- checkExpression _funDefType' _funDefBody
    let params = fst (unfoldFunType _funDefType')
    _funDefArgsInfo' <- checkArgsInfo params
    return
      FunctionDef
        { _funDefBody = _funDefBody',
          _funDefType = _funDefType',
          _funDefExamples = _funDefExamples',
          _funDefArgsInfo = _funDefArgsInfo',
          _funDefName,
          _funDefTerminating,
          _funDefInstance,
          _funDefCoercion,
          _funDefBuiltin,
          _funDefPragmas
        }
  when _funDefInstance $
    checkInstanceType funDef
  when _funDefCoercion $
    checkCoercionType funDef
  registerFunctionDef funDef
  rememberFunctionDef funDef
  return funDef
  where
    -- Since default arguments come from the left of the : then it must be that
    -- there are at least n FunctionParameter
    checkArgsInfo :: [FunctionParameter] -> Sem r [ArgInfo]
    checkArgsInfo allparams = execOutputList $ do
      go (zipExact infos params)
      where
        params = take n allparams
        infos = _funDefArgsInfo
        n = length infos
        go :: [(ArgInfo, FunctionParameter)] -> Sem (Output ArgInfo ': r) ()
        go = \case
          [] -> return ()
          (me, p) : rest -> do
            me' <- traverseOf (argInfoDefault . _Just) (checkExpression (p ^. paramType)) me
            output me'
            withLocalTypeMaybe (p ^. paramName) (p ^. paramType) (go rest)

checkIsType ::
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable, Termination, Output TypedHole, Output CastHole] r) =>
  Interval ->
  Expression ->
  Sem r Expression
checkIsType = checkExpression . smallUniverseE

checkDefType ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable, Termination, Output TypedHole, Output CastHole] r) =>
  Expression ->
  Sem r Expression
checkDefType ty = checkIsType loc ty
  where
    loc = getLoc ty

checkInstanceType ::
  forall r.
  (Members '[Error TypeCheckerError, Reader InfoTable, Inference, NameIdGen] r) =>
  FunctionDef ->
  Sem r ()
checkInstanceType FunctionDef {..} = case mi of
  Just ii@InstanceInfo {..} -> do
    tab <- ask
    unless (isTrait tab _instanceInfoInductive) $
      throw (ErrTargetNotATrait (TargetNotATrait _funDefType))
    is <- subsumingInstances (tab ^. infoInstances) ii
    unless (null is) $
      throw (ErrSubsumedInstance (SubsumedInstance ii is (getLoc _funDefName)))
    let metaVars = HashSet.fromList $ mapMaybe (^. paramName) _instanceInfoArgs
    mapM_ (checkArg tab metaVars ii) _instanceInfoArgs
  Nothing ->
    throw (ErrInvalidInstanceType (InvalidInstanceType _funDefType))
  where
    mi =
      instanceFromTypedExpression
        ( TypedExpression
            { _typedType = _funDefType,
              _typedExpression = ExpressionIden (IdenFunction _funDefName)
            }
        )

    checkArg :: InfoTable -> HashSet VarName -> InstanceInfo -> FunctionParameter -> Sem r ()
    checkArg tab metaVars ii fp@FunctionParameter {..} = case _paramImplicit of
      Implicit -> return ()
      Explicit -> throw (ErrExplicitInstanceArgument (ExplicitInstanceArgument fp))
      ImplicitInstance -> case traitFromExpression metaVars _paramType of
        Just app@InstanceApp {..}
          | isTrait tab _instanceAppHead ->
              checkTraitTermination app ii
        _ ->
          throw (ErrNotATrait (NotATrait _paramType))

checkInstanceParam :: (Member (Error TypeCheckerError) r) => InfoTable -> Expression -> Sem r ()
checkInstanceParam tab ty = case traitFromExpression mempty ty of
  Just InstanceApp {..} | isTrait tab _instanceAppHead -> return ()
  _ -> throw (ErrNotATrait (NotATrait ty))

checkCoercionType ::
  forall r.
  (Members '[Error TypeCheckerError, Reader InfoTable, Inference] r) =>
  FunctionDef ->
  Sem r ()
checkCoercionType FunctionDef {..} = case mi of
  Just CoercionInfo {..} -> do
    tab <- ask
    unless (isTrait tab _coercionInfoInductive) $
      throw (ErrTargetNotATrait (TargetNotATrait _funDefType))
    unless (isTrait tab (_coercionInfoTarget ^. instanceAppHead)) $
      throw (ErrInvalidCoercionType (InvalidCoercionType _funDefType))
    mapM_ checkArg _coercionInfoArgs
  Nothing ->
    throw (ErrInvalidCoercionType (InvalidCoercionType _funDefType))
  where
    mi =
      coercionFromTypedExpression
        ( TypedExpression
            { _typedType = _funDefType,
              _typedExpression = ExpressionIden (IdenFunction _funDefName)
            }
        )

    checkArg :: FunctionParameter -> Sem r ()
    checkArg fp@FunctionParameter {..} = case _paramImplicit of
      Implicit -> return ()
      Explicit -> throw (ErrWrongCoercionArgument (WrongCoercionArgument fp))
      ImplicitInstance -> throw (ErrWrongCoercionArgument (WrongCoercionArgument fp))

checkExample ::
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Output Example, State TypesTable, Termination] r) =>
  Example ->
  Sem r Example
checkExample e = do
  e' <- withEmptyVars (runInferenceDef (traverseOf exampleExpression (fmap (^. typedExpression) . inferExpression Nothing >=> strongNormalize) e))
  output e'
  return e'

checkExpression ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Reader LocalVars, Inference, Output Example, Output TypedHole, Output CastHole, State TypesTable, Termination] r) =>
  Expression ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  e' <- inferExpression' (Just expectedTy) e
  let inferredType = e' ^. typedType
  whenJustM (matchTypes expectedTy inferredType) (const (err inferredType))
  return (e' ^. typedExpression)
  where
    err :: Expression -> Sem r a
    err inferred = do
      inferred' <- strongNormalize inferred
      expected' <- strongNormalize expectedTy
      throw $
        ErrWrongType
          ( WrongType
              { _wrongTypeThing = Left e,
                _wrongTypeThingWithHoles = Nothing,
                _wrongTypeActual = inferred',
                _wrongTypeExpected = expected'
              }
          )

resolveCastHoles ::
  forall a r.
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Inference, Output Example, Output TypedHole, State TypesTable, Termination] r) =>
  Sem (Output CastHole ': r) a ->
  Sem r a
resolveCastHoles s = do
  (hs, e) <- runOutputList s
  let (hs1, hs2) = partition (isCastInt . (^. castHoleType)) hs
  mapM_ (go getIntTy) hs1
  mapM_ (go getNatTy) hs2
  return e
  where
    go :: (Interval -> Sem r Expression) -> CastHole -> Sem r ()
    go mkTy CastHole {..} = do
      m <- queryMetavarFinal _castHoleHole
      case m of
        Just {} -> return ()
        Nothing -> do
          ty <- mkTy (getLoc _castHoleHole)
          void (matchTypes (ExpressionHole _castHoleHole) ty)

    mkBuiltinInductive :: BuiltinInductive -> Interval -> Sem r Expression
    mkBuiltinInductive b i = fmap (ExpressionIden . IdenInductive) (getBuiltinName i b)

    getIntTy :: Interval -> Sem r Expression
    getIntTy = mkBuiltinInductive BuiltinInt

    getNatTy :: Interval -> Sem r Expression
    getNatTy = mkBuiltinInductive BuiltinNat

resolveInstanceHoles ::
  forall a r.
  (HasExpressions a) =>
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Inference, Output Example, State TypesTable, Termination] r) =>
  Sem (Output TypedHole ': r) a ->
  Sem r a
resolveInstanceHoles s = do
  (hs, e) <- runOutputList s
  ts <- mapM goResolve hs
  let subs = HashMap.fromList (zipExact (map (^. typedHoleHole) hs) ts)
  subsInstanceHoles subs e
  where
    goResolve :: TypedHole -> Sem r Expression
    goResolve h@TypedHole {..} = do
      t <- resolveTraitInstance h
      resolveInstanceHoles $ resolveCastHoles $ runReader _typedHoleLocalVars $ checkExpression _typedHoleType t

checkFunctionParameter ::
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable, Termination, Output TypedHole, Output CastHole] r) =>
  FunctionParameter ->
  Sem r FunctionParameter
checkFunctionParameter (FunctionParameter mv i e) = do
  e' <- checkIsType (getLoc e) e
  when (i == ImplicitInstance) $ do
    tab <- ask
    checkInstanceParam tab e'
  return (FunctionParameter mv i e')

checkConstructorDef ::
  ( Members
      '[ Reader EntryPoint,
         Reader InfoTable,
         Error TypeCheckerError,
         State NegativeTypeParameters
       ]
      r
  ) =>
  InductiveDef ->
  ConstructorDef ->
  Sem r ()
checkConstructorDef ty ctor = checkConstructorReturnType ty ctor

checkConstructorReturnType ::
  (Members '[Reader InfoTable, Error TypeCheckerError] r) =>
  InductiveDef ->
  ConstructorDef ->
  Sem r ()
checkConstructorReturnType indType ctor = do
  let ctorName = ctor ^. inductiveConstructorName
      tyName = indType ^. inductiveName
      indParams = map (^. inductiveParamName) (indType ^. inductiveParameters)
      ctorReturnType = snd (viewConstructorType (ctor ^. inductiveConstructorType))
      expectedReturnType =
        foldExplicitApplication
          (ExpressionIden (IdenInductive tyName))
          (map (ExpressionIden . IdenVar) indParams)
  when
    (ctorReturnType /= expectedReturnType)
    ( throw
        ( ErrWrongReturnType
            ( WrongReturnType
                { _wrongReturnTypeConstructorName = ctorName,
                  _wrongReturnTypeExpected = expectedReturnType,
                  _wrongReturnTypeActual = ctorReturnType
                }
            )
        )
    )

inferExpression ::
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable, Termination] r) =>
  Maybe Expression -> -- type hint
  Expression ->
  Sem r TypedExpression
inferExpression hint e = resolveInstanceHoles $ resolveCastHoles $ inferExpression' hint e

lookupVar :: (Members '[Reader LocalVars, Reader InfoTable] r) => Name -> Sem r Expression
lookupVar v = do
  locals <- asks (^. localTypes)
  return
    ( fromMaybe
        err
        ( locals ^. at v
        )
    )
  where
    err = error $ "internal error: could not find var " <> ppTrace v <> " at " <> ppTrace (getLoc v)

-- | helper function for function clauses and lambda functions
checkClause ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Builtins, Output Example, State TypesTable, Termination, Output TypedHole, Output CastHole] r) =>
  -- | Type
  Expression ->
  -- | Arguments
  [PatternArg] ->
  -- | Body
  Expression ->
  Sem r ([PatternArg], Expression) -- (Checked patterns, Checked body)
checkClause clauseType clausePats body = do
  locals0 <- ask
  (localsPats, (checkedPatterns, bodyType)) <- helper clausePats clauseType
  let locals' = locals0 <> localsPats
  bodyTy' <- substitutionE (localsToSubsE locals') bodyType
  checkedBody <- local (const locals') (checkExpression bodyTy' body)
  return (checkedPatterns, checkedBody)
  where
    helper :: [PatternArg] -> Expression -> Sem r (LocalVars, ([PatternArg], Expression))
    helper pats ty = runState emptyLocalVars (go pats ty)

    go :: [PatternArg] -> Expression -> Sem (State LocalVars ': r) ([PatternArg], Expression)
    go pats bodyTy = case pats of
      [] -> return ([], bodyTy)
      (p : ps) -> do
        bodyTy' <- weakNormalize bodyTy
        case bodyTy' of
          ExpressionHole h -> do
            fun <- holeRefineToFunction h
            go pats (ExpressionFunction fun)
          _ -> case unfoldFunType bodyTy' of
            ([], _) -> error "too many patterns"
            (par : pars, ret) -> do
              par' <- checkPattern par p
              first (par' :) <$> go ps (foldFunType pars ret)

-- | Refines a hole into a function type. I.e. '_@1' is matched with '_@fresh → _@fresh'
holeRefineToFunction :: (Members '[Inference, NameIdGen] r) => Hole -> Sem r Function
holeRefineToFunction h = do
  s <- queryMetavar h
  case s of
    Just h' -> case h' of
      ExpressionFunction f -> return f
      ExpressionHole h'' -> holeRefineToFunction h''
      _ -> error "cannot refine hole to function"
    Nothing -> do
      l <- ExpressionHole <$> freshHole (getLoc h)
      r <- ExpressionHole <$> freshHole (getLoc h)
      let fun = Function (unnamedParameter l) r
      whenJustM (matchTypes (ExpressionHole h) (ExpressionFunction fun)) impossible
      return fun

matchIsImplicit :: (Member (Error TypeCheckerError) r) => IsImplicit -> PatternArg -> Sem r ()
matchIsImplicit expected actual =
  unless
    (expected == actual ^. patternArgIsImplicit)
    ( throw
        ( ErrArityCheckerError
            ( ErrWrongPatternIsImplicit
                WrongPatternIsImplicit
                  { _wrongPatternIsImplicitExpected = expected,
                    _wrongPatternIsImplicitActual = actual
                  }
            )
        )
    )

checkPattern ::
  forall r.
  (Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars, Inference, NameIdGen, State FunctionsTable] r) =>
  FunctionParameter ->
  PatternArg ->
  Sem r PatternArg
checkPattern = go
  where
    go :: FunctionParameter -> PatternArg -> Sem r PatternArg
    go argTy patArg = do
      matchIsImplicit (argTy ^. paramImplicit) patArg
      tyVarMap <- localsToSubsE <$> get
      ty <- substitutionE tyVarMap (argTy ^. paramType)
      let pat = patArg ^. patternArgPattern
          name = patArg ^. patternArgName
      whenJust name (\n -> addVar n ty argTy)
      pat' <- case pat of
        PatternVariable v -> addVar v ty argTy $> pat
        PatternWildcardConstructor {} -> impossible
        PatternConstructorApp a -> do
          s <- checkSaturatedInductive ty
          info <- lookupConstructor (a ^. constrAppConstructor)
          let constrIndName = info ^. constructorInfoInductive
              constrName = a ^. constrAppConstructor
              err :: MatchError -> Sem r ()
              err m =
                throw
                  ( ErrWrongType
                      WrongType
                        { _wrongTypeThing = Right pat,
                          _wrongTypeThingWithHoles = Nothing,
                          _wrongTypeExpected = m ^. matchErrorRight,
                          _wrongTypeActual = m ^. matchErrorLeft
                        }
                  )
          case s of
            Left hole -> do
              let indParams = info ^. constructorInfoInductiveParameters
                  numIndParams = length indParams
                  indName :: Iden
                  indName = IdenInductive (info ^. constructorInfoInductive)
                  loc = getLoc a
              paramHoles <- map ExpressionHole <$> replicateM numIndParams (freshHole loc)
              let patternTy = foldApplication (ExpressionIden indName) (map (ApplicationArg Explicit) paramHoles)
              whenJustM
                (matchTypes patternTy (ExpressionHole hole))
                err
              let tyArgs = zipExact indParams paramHoles
              PatternConstructorApp <$> goConstr indName a tyArgs
            Right (ind, tyArgs) -> do
              when
                (ind /= constrIndName)
                ( throw
                    ( ErrWrongConstructorType
                        WrongConstructorType
                          { _wrongCtorTypeName = constrName,
                            _wrongCtorTypeExpected = ind,
                            _wrongCtorTypeActual = constrIndName
                          }
                    )
                )
              PatternConstructorApp <$> goConstr (IdenInductive ind) a tyArgs
      return (set patternArgPattern pat' patArg)
      where
        addVar :: VarName -> Expression -> FunctionParameter -> Sem r ()
        addVar v ty argType = do
          modify (addType v ty)
          registerIdenType v ty
          whenJust (argType ^. paramName) (\v' -> modify (addTypeMapping v' v))
        goConstr :: Iden -> ConstructorApp -> [(InductiveParameter, Expression)] -> Sem r ConstructorApp
        goConstr inductivename app@(ConstructorApp c ps _) ctx = do
          (_, psTys) <- constructorArgTypes <$> lookupConstructor c
          psTys' <- mapM (substituteIndParams ctx) psTys
          let expectedNum = length psTys
              w = map unnamedParameter psTys'
          when (expectedNum /= length ps) (throw (appErr app expectedNum))
          pis <- zipWithM go w ps
          let appTy = foldExplicitApplication (ExpressionIden inductivename) (map snd ctx)
          return app {_constrAppType = Just appTy, _constrAppParameters = pis}
        appErr :: ConstructorApp -> Int -> TypeCheckerError
        appErr app expected =
          ErrArityCheckerError
            ( ErrWrongConstructorAppLength
                ( WrongConstructorAppLength
                    { _wrongConstructorAppLength = app,
                      _wrongConstructorAppLengthExpected = expected
                    }
                )
            )

    checkSaturatedInductive :: Expression -> Sem r (Either Hole (InductiveName, [(InductiveParameter, Expression)]))
    checkSaturatedInductive ty = do
      i <- viewInductiveApp ty
      case i of
        Left hole -> return (Left hole)
        Right (ind, args) -> do
          params :: [InductiveParameter] <-
            (^. inductiveInfoDef . inductiveParameters)
              <$> lookupInductive ind
          let numArgs = length args
              numParams = length params
          when
            (numArgs < numParams)
            ( throw
                ( ErrTooFewArgumentsIndType
                    ( WrongNumberArgumentsIndType
                        { _wrongNumberArgumentsIndTypeActualType = ty,
                          _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                          _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                        }
                    )
                )
            )
          when
            (numArgs > numParams)
            ( throw
                ( ErrTooManyArgumentsIndType
                    ( WrongNumberArgumentsIndType
                        { _wrongNumberArgumentsIndTypeActualType = ty,
                          _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                          _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                        }
                    )
                )
            )
          return (Right (ind, zipExact params args))

inferExpression' ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, State TypesTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Output Example, Output TypedHole, Output CastHole, Builtins, Termination] r) =>
  Maybe Expression ->
  Expression ->
  Sem r TypedExpression
inferExpression' hint e = case e of
  ExpressionIden i -> goIden i
  ExpressionApplication a -> goApplication a
  ExpressionLiteral l -> goLiteral l
  ExpressionFunction f -> goFunction f
  ExpressionHole h -> goHole h
  ExpressionInstanceHole h -> goInstanceHole h
  ExpressionUniverse u -> goUniverse u
  ExpressionSimpleLambda l -> goSimpleLambda l
  ExpressionLambda l -> goLambda l
  ExpressionLet l -> goLet l
  ExpressionCase l -> goCase l
  where
    goLet :: Let -> Sem r TypedExpression
    goLet l = do
      _letClauses <- mapM goLetClause (l ^. letClauses)
      typedBody <- inferExpression' hint (l ^. letExpression)
      return
        TypedExpression
          { _typedType = typedBody ^. typedType,
            _typedExpression =
              ExpressionLet
                Let
                  { _letExpression = typedBody ^. typedExpression,
                    _letClauses
                  }
          }

    goLetClause :: LetClause -> Sem r LetClause
    goLetClause = \case
      LetFunDef f -> LetFunDef <$> checkFunctionDef f
      LetMutualBlock b -> LetMutualBlock <$> goMutualLet b
      where
        goMutualLet :: MutualBlockLet -> Sem r MutualBlockLet
        goMutualLet (MutualBlockLet fs) = MutualBlockLet <$> mapM checkFunctionDef fs

    goHole :: Hole -> Sem r TypedExpression
    goHole h = do
      void (queryMetavar h)
      return
        TypedExpression
          { _typedExpression = ExpressionHole h,
            _typedType = ExpressionUniverse (SmallUniverse (getLoc h))
          }

    goInstanceHole :: InstanceHole -> Sem r TypedExpression
    goInstanceHole h = do
      let ty = fromMaybe impossible hint
      locals <- ask
      output (TypedHole h ty locals)
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionInstanceHole h
          }

    goSimpleLambda :: SimpleLambda -> Sem r TypedExpression
    goSimpleLambda (SimpleLambda (SimpleBinder v ty) b) = do
      b' <- inferExpression' Nothing b
      let smallUni = smallUniverseE (getLoc ty)
      ty' <- checkExpression smallUni ty
      let fun = Function (unnamedParameter smallUni) (b' ^. typedType)
      return
        TypedExpression
          { _typedType = ExpressionFunction fun,
            _typedExpression = ExpressionSimpleLambda (SimpleLambda (SimpleBinder v ty') (b' ^. typedExpression))
          }

    goCase :: Case -> Sem r TypedExpression
    goCase c = do
      ty <- case hint of
        Nothing -> ExpressionHole <$> freshHole (getLoc c)
        Just hi -> return hi
      typedCaseExpression <- inferExpression' Nothing (c ^. caseExpression)
      let _caseExpression = typedCaseExpression ^. typedExpression
          _caseExpressionType = Just (typedCaseExpression ^. typedType)
          _caseExpressionWholeType = Just ty
          goBranch :: CaseBranch -> Sem r CaseBranch
          goBranch b = do
            (onePat, _caseBranchExpression) <- checkClause funty [b ^. caseBranchPattern] (b ^. caseBranchExpression)
            let _caseBranchPattern = case onePat of
                  [x] -> x
                  _ -> impossible
            return CaseBranch {..}
            where
              funty :: Expression
              funty = ExpressionFunction (mkFunction (typedCaseExpression ^. typedType) ty)
      _caseBranches <- mapM goBranch (c ^. caseBranches)
      let _caseParens = c ^. caseParens
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionCase Case {..}
          }

    goLambda :: Lambda -> Sem r TypedExpression
    goLambda l = do
      ty <- case hint of
        Just hi -> return hi
        Nothing -> ExpressionHole <$> freshHole (getLoc l)
      _lambdaClauses <- mapM (goClause ty) (l ^. lambdaClauses)
      let _lambdaType = Just ty
          l' = Lambda {..}
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionLambda l'
          }
      where
        goClause :: Expression -> LambdaClause -> Sem r LambdaClause
        goClause ty (LambdaClause pats body) = do
          (pats', body') <- checkClause ty (toList pats) body
          return (LambdaClause (nonEmpty' pats') body')

    goUniverse :: SmallUniverse -> Sem r TypedExpression
    goUniverse u =
      return
        TypedExpression
          { _typedType = ExpressionUniverse u,
            _typedExpression = ExpressionUniverse u
          }

    goFunction :: Function -> Sem r TypedExpression
    goFunction (Function l r) = do
      let uni = smallUniverseE (getLoc l)
      l' <- checkFunctionParameter l
      let bodyEnv :: Sem r a -> Sem r a
          bodyEnv = withLocalTypeMaybe (l ^. paramName) (l ^. paramType)
      r' <- bodyEnv (checkExpression uni r)
      return (TypedExpression uni (ExpressionFunction (Function l' r')))

    goLiteral :: LiteralLoc -> Sem r TypedExpression
    goLiteral lit@(WithLoc i l) = do
      case l of
        LitNumeric v -> outHole v >> typedLitNumeric v
        LitInteger {} -> do
          ty <- getIntTy
          return $
            TypedExpression
              { _typedType = ty,
                _typedExpression = ExpressionLiteral lit
              }
        LitNatural {} -> do
          ty <- getNatTy
          return $
            TypedExpression
              { _typedType = ty,
                _typedExpression = ExpressionLiteral lit
              }
        LitString {} -> do
          str <- getBuiltinName i BuiltinString
          return
            TypedExpression
              { _typedExpression = ExpressionLiteral lit,
                _typedType = ExpressionIden (IdenAxiom str)
              }
      where
        typedLitNumeric :: Integer -> Sem r TypedExpression
        typedLitNumeric v
          | v < 0 = getIntTy >>= typedLit LitInteger BuiltinFromInt
          | otherwise = getNatTy >>= typedLit LitNatural BuiltinFromNat
          where
            typedLit :: (Integer -> Literal) -> BuiltinFunction -> Expression -> Sem r TypedExpression
            typedLit litt blt ty = do
              from <- getBuiltinName i blt
              ihole <- freshInstanceHole i
              let ty' = fromMaybe ty hint
              inferExpression' (Just ty') $
                foldApplication
                  (ExpressionIden (IdenFunction from))
                  [ ApplicationArg Implicit ty',
                    ApplicationArg ImplicitInstance (ExpressionInstanceHole ihole),
                    ApplicationArg Explicit (ExpressionLiteral (WithLoc i (litt v)))
                  ]

        mkBuiltinInductive :: BuiltinInductive -> Sem r Expression
        mkBuiltinInductive = fmap (ExpressionIden . IdenInductive) . getBuiltinName i

        getIntTy :: Sem r Expression
        getIntTy = mkBuiltinInductive BuiltinInt

        getNatTy :: Sem r Expression
        getNatTy = mkBuiltinInductive BuiltinNat

        outHole :: Integer -> Sem r ()
        outHole v
          | v < 0 = case hint of
              Just (ExpressionHole h) ->
                output CastHole {_castHoleHole = h, _castHoleType = CastInt}
              _ ->
                return ()
          | otherwise = case hint of
              Just (ExpressionHole h) ->
                output CastHole {_castHoleHole = h, _castHoleType = CastNat}
              _ ->
                return ()

    goIden :: Iden -> Sem r TypedExpression
    goIden i = case i of
      IdenFunction fun -> do
        info <- lookupFunction fun
        return (TypedExpression (info ^. functionInfoDef . funDefType) (ExpressionIden i))
      IdenConstructor c -> do
        ty <- lookupConstructorType c
        return (TypedExpression ty (ExpressionIden i))
      IdenVar v -> do
        ty <- lookupVar v
        return (TypedExpression ty (ExpressionIden i))
      IdenAxiom v -> do
        info <- lookupAxiom v
        return (TypedExpression (info ^. axiomInfoDef . axiomType) (ExpressionIden i))
      IdenInductive v -> do
        kind <- lookupInductiveType v
        return (TypedExpression kind (ExpressionIden i))

    goApplication :: Application -> Sem r TypedExpression
    goApplication (Application l r iapp) = inferExpression' Nothing l >>= helper
      where
        helper :: TypedExpression -> Sem r TypedExpression
        helper l' = do
          l'ty <- weakNormalize (l' ^. typedType)
          case l'ty of
            ExpressionFunction (Function (FunctionParameter paraName ifun funL) funR) -> do
              r' <- checkExpression funL r
              unless
                (iapp == ifun)
                ( error
                    ( "Impossible: implicitness mismatch"
                        <> show ifun
                        <> show iapp
                        <> "\n"
                        <> ppTrace (Application l r iapp)
                    )
                )
              ty <- substitutionE (substitutionApp (paraName, r')) funR
              return
                TypedExpression
                  { _typedExpression =
                      ExpressionApplication
                        Application
                          { _appLeft = l' ^. typedExpression,
                            _appRight = r',
                            _appImplicit = iapp
                          },
                    _typedType = ty
                  }
            ExpressionHole h -> do
              fun <- ExpressionFunction <$> holeRefineToFunction h
              helper (set typedType fun l')
            _ -> throw tyErr
              where
                tyErr :: TypeCheckerError
                tyErr =
                  ErrExpectedFunctionType
                    ( ExpectedFunctionType
                        { _expectedFunctionTypeExpression = e,
                          _expectedFunctionTypeLeft = l,
                          _expectedFunctionTypeType = l' ^. typedType
                        }
                    )

viewInductiveApp ::
  (Members '[Error TypeCheckerError, Inference, State FunctionsTable] r) =>
  Expression ->
  Sem r (Either Hole (InductiveName, [Expression]))
viewInductiveApp ty = do
  ty' <- weakNormalize ty
  let (t, as) = viewTypeApp ty'
  case t of
    ExpressionIden (IdenInductive n) -> return (Right (n, as))
    ExpressionHole h -> do
      r <- queryMetavar h
      case r of
        Just h' -> viewInductiveApp h'
        Nothing -> return (Left h)
    _ -> throw (ErrInvalidPatternMatching (InvalidPatternMatching ty))
  where
    viewTypeApp :: Expression -> (Expression, [Expression])
    viewTypeApp tyapp = case tyapp of
      ExpressionApplication (Application l r _) ->
        second (`snoc` r) (viewTypeApp l)
      _ -> (tyapp, [])
