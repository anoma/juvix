module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error,
    checkModule,
    checkTable,
    checkModuleIndex,
    checkModuleNoCache,
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
import Juvix.Compiler.Internal.Extra hiding (freshHole)
import Juvix.Compiler.Internal.Extra qualified as Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (Termination)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew.Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Resolver
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Termination
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

type MCache = Cache ModuleIndex Module

data AppBuilder = AppBuilder
  { _appBuilder :: Expression,
    _appBuilderType :: Expression,
    _appBuilderArgs :: [ApplicationArg]
  }

makeLenses ''AppBuilder

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
  (Members '[HighlightBuilder, Reader EntryPoint, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, NameIdGen, State TypesTable, State NegativeTypeParameters, Output Example, Builtins, Termination, Output TypedHole, Output CastHole, Reader LocalVars] r) =>
  InductiveDef ->
  Sem r InductiveDef
checkInductiveDef InductiveDef {..} = runInferenceDef $ do
  params <- checkParams
  withLocalTypes params $ do
    constrs' <- mapM goConstructor _inductiveConstructors
    ty <- lookupInductiveType _inductiveName
    registerNameIdType (_inductiveName ^. nameId) ty
    examples' <- mapM checkExample _inductiveExamples
    inductiveType' <- checkDefType _inductiveType
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
    checkParams :: Sem (Inference ': r) [(Name, Expression)]
    checkParams = mapM checkParam _inductiveParameters
      where
      checkParam :: InductiveParameter -> Sem (Inference ': r) (Name, Expression)
      checkParam p = do
        ty' <- checkIsType (getLoc p) (p ^. inductiveParamType)
        return (p ^. inductiveParamName,  ty')

    goConstructor :: ConstructorDef -> Sem (Inference ': r) ConstructorDef
    goConstructor ConstructorDef {..} = do
      expectedRetTy <- lookupConstructorReturnType _inductiveConstructorName
      cty' <- checkIsType (getLoc _inductiveConstructorType) _inductiveConstructorType
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

checkMutualStatement ::
  (Members '[HighlightBuilder, State NegativeTypeParameters, Reader EntryPoint, Inference, Reader LocalVars, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, Termination] r) =>
  MutualStatement ->
  Sem r MutualStatement
checkMutualStatement = \case
  StatementFunction f -> do
    -- traceM ("checkstatement f " <> ppTrace f)
    f' <- resolveInstanceHoles (resolveCastHoles (checkFunctionDef f))
    -- traceM ("checkstatement f' " <> ppTrace f')
    return (StatementFunction f')
  StatementInductive f -> StatementInductive <$> resolveInstanceHoles (resolveCastHoles (checkInductiveDef f))
  StatementAxiom ax -> do
    registerNameIdType (ax ^. axiomName . nameId) (ax ^. axiomType)
    return $ StatementAxiom ax

unfoldFunType1' :: (Members '[Inference] r) => Expression -> Sem r (Maybe (FunctionParameter, Expression))
unfoldFunType1' =
  weakNormalize
    >=> \case
      ExpressionFunction (Function l r) -> return (Just (l, r))
      _ -> return Nothing

unfoldFunType' :: (Members '[Inference] r) => Expression -> Sem r ([FunctionParameter], Expression)
unfoldFunType' e = do
  e' <- unfoldFunType1' e
  case e' of
    Just (l, r) -> first (l :) <$> unfoldFunType' r
    _ -> return ([], e)

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
    _funDefBody' <- checkFunctionBody _funDefType' _funDefBody
    params <- fst <$> unfoldFunType' _funDefType'
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
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Reader LocalVars, Inference, Output Example, Output TypedHole, State TypesTable, Termination, Output CastHole] r) =>
  Expression ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  e' <- inferExpression' (Just expectedTy) e
  let inferredType = e' ^. typedType
  whenJustM (matchTypes expectedTy inferredType) (const (err e'))
  return (e' ^. typedExpression)
  where
    err :: TypedExpression -> Sem r a
    err inferred = do
      e' <- strongNormalize e
      inferred' <- strongNormalize (inferred ^. typedType)
      expected' <- strongNormalize expectedTy
      throw
        . ErrWrongType
        $ WrongType
          { _wrongTypeThing = Left e',
            _wrongTypeThingWithHoles = Just (Left (inferred ^. typedExpression)),
            _wrongTypeActual = inferred',
            _wrongTypeExpected = expected'
          }

resolveInstanceHoles ::
  forall a r.
  (HasExpressions a) =>
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Inference, Output Example, State TypesTable, Termination] r) =>
  Sem (Output TypedHole ': r) a ->
  Sem r a
resolveInstanceHoles s = do
  (hs, e) <- runOutputList s
  -- traceM ("holes: " <> ppTrace hs)
  ts <- mapM goResolve hs
  let subs = HashMap.fromList (zipExact (map (^. typedHoleHole) hs) ts)
  -- traceM
  --   ( "apply subs: "
  --       <> ppTrace (HashMap.toList subs)
  --       <> "\nto "
  --       <> ppTrace e
  --       <> "\nequals "
  --       <> ppTrace (subsHoles subs e)
  --   )
  subsInstanceHoles subs e
  where
    goResolve :: TypedHole -> Sem r Expression
    goResolve h@TypedHole {..} = do
      t <- resolveTraitInstance h
      resolveInstanceHoles
        . resolveCastHoles
        . runReader _typedHoleLocalVars
        $ checkExpression _typedHoleType t

checkFunctionParameter ::
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable, Termination, Output TypedHole, Output CastHole] r) =>
  FunctionParameter ->
  Sem r FunctionParameter
checkFunctionParameter FunctionParameter {..} = do
  let ty = _paramType
  ty' <- checkIsType (getLoc ty) ty
  when (_paramImplicit == ImplicitInstance) $ do
    tab <- ask
    checkInstanceParam tab ty'
  return
    FunctionParameter
      { _paramType = ty',
        _paramName,
        _paramImplicit
      }

inferExpression ::
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable, Termination] r) =>
  -- | type hint
  Maybe Expression ->
  Expression ->
  Sem r TypedExpression
inferExpression hint = resolveInstanceHoles . resolveCastHoles . inferExpression' hint

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

checkFunctionBody ::
  (Members '[Reader LocalVars, Reader InfoTable, NameIdGen, Error TypeCheckerError, Output Example, Output TypedHole, State TypesTable, State HighlightInput, State FunctionsTable, Builtins, Inference, Termination, Output CastHole] r) =>
  Expression ->
  Expression ->
  Sem r Expression
checkFunctionBody expectedTy body =
  case body of
    ExpressionLambda {} -> checkExpression expectedTy body
    _ -> do
      (patterns', typedBody) <- checkClause (getLoc body) expectedTy [] body
      return $ case nonEmpty patterns' of
        Nothing -> typedBody
        Just lambdaPatterns' ->
          ExpressionLambda
            Lambda
              { _lambdaType = Nothing,
                _lambdaClauses =
                  pure
                    LambdaClause
                      { _lambdaPatterns = lambdaPatterns',
                        _lambdaBody = typedBody
                      }
              }

-- | helper function for lambda functions and case branches
checkClause ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Builtins, Output Example, State TypesTable, Termination, Output TypedHole, Output CastHole] r) =>
  Interval ->
  -- | Type
  Expression ->
  -- | Arguments
  [PatternArg] ->
  -- | Body
  Expression ->
  Sem r ([PatternArg], Expression) -- (Checked patterns, Checked body)
checkClause clauseLoc clauseType clausePats body = do
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
      [] -> do
        (bodyParams, bodyRest) <- unfoldFunType' bodyTy
        guessedBodyParams <- unfoldArity <$> guessArity body
        let pref' :: [IsImplicit] = map (^. paramImplicit) (take pref bodyParams)
            pref :: Int = aI - targetI
            preImplicits = length . takeWhile isImplicitOrInstance
            aI :: Int = preImplicits (map (^. paramImplicit) bodyParams)
            targetI :: Int = preImplicits (map (^. arityParameterImplicit) guessedBodyParams)
        if
            | 0 < pref -> do
                let n = length pref'
                    bodyParams' = drop n bodyParams
                    ty' = foldFunType bodyParams' bodyRest
                wildcards <- mapM (genWildcard clauseLoc) pref'
                return (wildcards, ty')
            | otherwise -> do
                return ([], bodyTy)
      p : ps -> do
        bodyTy' <- weakNormalize bodyTy
        case bodyTy' of
          ExpressionHole h -> do
            fun <- holeRefineToFunction (p ^. patternArgIsImplicit) h
            go pats (ExpressionFunction fun)
          _ -> do
            unfoldedBodyTy' <- unfoldFunType' bodyTy'
            case unfoldedBodyTy' of
              ([], _) -> throwTooManyPatterns
              (par : pars, ret) -> do
                let checkPatternAndContinue = do
                      par' <- checkPattern par p
                      first (par' :) <$> go ps (foldFunType pars ret)

                    loc :: Interval
                    loc = getLoc par

                    insertWildcard :: IsImplicit -> Sem (State LocalVars ': r) ([PatternArg], Expression)
                    insertWildcard impl = do
                      w <- genWildcard loc impl
                      go (w : p : ps) bodyTy'

                case (p ^. patternArgIsImplicit, par ^. paramImplicit) of
                  (Explicit, Explicit) -> checkPatternAndContinue
                  (Implicit, Implicit) -> checkPatternAndContinue
                  (ImplicitInstance, ImplicitInstance) -> checkPatternAndContinue
                  (Implicit, Explicit) -> throwWrongIsImplicit p Implicit
                  (ImplicitInstance, Explicit) -> throwWrongIsImplicit p ImplicitInstance
                  (Explicit, Implicit) -> insertWildcard Implicit
                  (ImplicitInstance, Implicit) -> insertWildcard Implicit
                  (Explicit, ImplicitInstance) -> insertWildcard ImplicitInstance
                  (Implicit, ImplicitInstance) -> insertWildcard ImplicitInstance
        where
          throwWrongIsImplicit :: (Members '[Error TypeCheckerError] r') => PatternArg -> IsImplicit -> Sem r' a
          throwWrongIsImplicit patArg expected =
            throw . ErrArityCheckerError $
              ErrWrongPatternIsImplicit
                WrongPatternIsImplicit
                  { _wrongPatternIsImplicitActual = patArg,
                    _wrongPatternIsImplicitExpected = expected
                  }
          throwTooManyPatterns :: (Members '[Error TypeCheckerError] r') => Sem r' a
          throwTooManyPatterns =
            throw . ErrArityCheckerError $
              ErrLhsTooManyPatterns
                LhsTooManyPatterns
                  { _lhsTooManyPatternsRemaining = p :| ps
                  }

freshHoleImpl :: (Members '[NameIdGen] r) => Interval -> IsImplicit -> Sem r Expression
freshHoleImpl loc = \case
  Explicit -> ExpressionHole <$> Extra.freshHole loc
  Implicit -> ExpressionHole <$> Extra.freshHole loc
  ImplicitInstance -> ExpressionInstanceHole <$> Extra.freshInstanceHole loc

-- | Refines a hole into a function type. I.e. '_@1' is matched with '_@fresh → _@fresh'
holeRefineToFunction :: (Members '[Inference, NameIdGen] r) => IsImplicit -> Hole -> Sem r Function
holeRefineToFunction impl h = do
  s <- queryMetavar h
  case s of
    Just h' -> case h' of
      ExpressionFunction f -> return f
      ExpressionHole h'' -> holeRefineToFunction impl h''
      _ -> error "cannot refine hole to function"
    Nothing -> do
      l <- freshHoleImpl (getLoc h) impl
      r <- freshHoleImpl (getLoc h) Implicit
      let fun = Function (unnamedParameter' impl l) r
      whenJustM (matchTypes (ExpressionHole h) (ExpressionFunction fun)) impossible
      return fun

matchIsImplicit :: (Member (Error TypeCheckerError) r) => IsImplicit -> PatternArg -> Sem r ()
matchIsImplicit expected actual =
  unless
    (expected == actual ^. patternArgIsImplicit)
    . throw
    . ErrArity
    $ ErrWrongPatternIsImplicit
      WrongPatternIsImplicit
        { _wrongPatternIsImplicitExpected = expected,
          _wrongPatternIsImplicitActual = actual
        }

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
      tyVarMap <- fmap (ExpressionIden . IdenVar) . (^. localTyMap) <$> get
      ty <- substitutionE tyVarMap (argTy ^. paramType)
      let pat = patArg ^. patternArgPattern
          name = patArg ^. patternArgName
      whenJust name (\n -> addVar n ty argTy)
      pat' <- case pat of
        PatternVariable v -> addVar v ty argTy $> pat
        PatternWildcardConstructor {} -> impossible
        PatternConstructorApp a -> goPatternConstructor pat ty a
      return (set patternArgPattern pat' patArg)
      where
        goPatternConstructor :: Pattern -> Expression -> ConstructorApp -> Sem r Pattern
        goPatternConstructor pat ty a = do
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
              paramHoles <- replicateM numIndParams (freshHoleImpl loc Implicit)
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
          ErrArity
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
            ( throw $
                ErrTooFewArgumentsIndType
                  WrongNumberArgumentsIndType
                    { _wrongNumberArgumentsIndTypeActualType = ty,
                      _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                      _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                    }
            )
          when
            (numArgs > numParams)
            ( throw $
                ErrTooManyArgumentsIndType
                  WrongNumberArgumentsIndType
                    { _wrongNumberArgumentsIndTypeActualType = ty,
                      _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                      _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                    }
            )
          return (Right (ind, zipExact params args))

inferExpression' ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, State TypesTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Output Example, Output TypedHole, Builtins, Termination, Output CastHole] r) =>
  Maybe Expression ->
  Expression ->
  Sem r TypedExpression
inferExpression' = holesHelper

-- | Checks anything but an Application. Does not insert holes
inferLeftAppExpression ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, State TypesTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Output Example, Output TypedHole, Builtins, Termination, Output CastHole] r) =>
  Maybe Expression ->
  Expression ->
  Sem r TypedExpression
inferLeftAppExpression mhint e = case e of
  ExpressionApplication {} -> impossible
  ExpressionIden i -> goIden i
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
      typedBody <- inferExpression' mhint (l ^. letExpression)
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
      ty <- maybe (freshHoleImpl (getLoc h) Implicit) return mhint
      return
        TypedExpression
          { _typedExpression = ExpressionHole h,
            _typedType = ty
          }

    goInstanceHole :: InstanceHole -> Sem r TypedExpression
    goInstanceHole h = do
      let ty = fromMaybe impossible mhint
      locals <- ask
      output (TypedHole h ty locals)
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionInstanceHole h
          }

    goSimpleLambda :: SimpleLambda -> Sem r TypedExpression
    goSimpleLambda (SimpleLambda (SimpleBinder v ty) b) = do
      b' <- withLocalType v ty (inferExpression' Nothing b)
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
      ty <- case mhint of
        Nothing -> freshHoleImpl (getLoc c) Implicit
        Just hi -> return hi
      typedCaseExpression <- inferExpression' Nothing (c ^. caseExpression)
      let _caseExpression = typedCaseExpression ^. typedExpression
          _caseExpressionType = Just (typedCaseExpression ^. typedType)
          _caseExpressionWholeType = Just ty
          goBranch :: CaseBranch -> Sem r CaseBranch
          goBranch b = do
            (onePat, _caseBranchExpression) <- checkClause (getLoc b) funty [b ^. caseBranchPattern] (b ^. caseBranchExpression)
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
      ty <- case mhint of
        Just hi -> return hi
        Nothing -> freshHoleImpl (getLoc l) Implicit
      _lambdaClauses <- mapM (goClause ty) (l ^. lambdaClauses)
      let lty' = Just ty
          l' =
            Lambda
              { _lambdaType = lty',
                _lambdaClauses
              }
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionLambda l'
          }
      where
        goClause :: Expression -> LambdaClause -> Sem r LambdaClause
        goClause ty cl@LambdaClause {..} = do
          (pats', body') <- checkClause (getLoc cl) ty (toList _lambdaPatterns) _lambdaBody
          return
            LambdaClause
              { _lambdaPatterns = nonEmpty' pats',
                _lambdaBody = body'
              }

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
    goLiteral lit@(WithLoc i l) = case l of
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
              ihole <- freshHoleImpl i ImplicitInstance
              let ty' = fromMaybe ty mhint
              inferExpression' (Just ty') $
                foldApplication
                  (ExpressionIden (IdenFunction from))
                  [ ApplicationArg Implicit ty',
                    ApplicationArg ImplicitInstance ihole,
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
          | v < 0 = case mhint of
              Just (ExpressionHole h) ->
                output CastHole {_castHoleHole = h, _castHoleType = CastInt}
              _ ->
                return ()
          | otherwise = case mhint of
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

-- | The hint is used for trailing holes only
holesHelper :: forall r. (Members '[HighlightBuilder, Reader InfoTable, State FunctionsTable, State TypesTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Output Example, Output TypedHole, Builtins, Termination, Output CastHole] r) => Maybe Expression -> Expression -> Sem r TypedExpression
holesHelper mhint expr = do
  -- traceM ("holes helper " <> ppTrace expr)
  -- (f, args) <- unfoldExpressionApp <$> weakNormalize expr
  -- TODO investigate why normalizing here gives problems with simple lambda
  let (f, args) = unfoldExpressionApp expr
  -- traceM ("holes helper left part " <> ppTrace f <> " args: " <> ppTrace args)
  -- let (f, args) = unfoldExpressionApp expr
  let hint
        | null args = mhint
        | otherwise = Nothing
  fTy <- inferLeftAppExpression hint f
  let iniBuilder =
        AppBuilder
          { _appBuilder = fTy ^. typedExpression,
            _appBuilderType = fTy ^. typedType,
            _appBuilderArgs = args
          }
  -- traceM
  --   ( "\n\nHolesHelper\n"
  --       <> "Builder: "
  --       <> ppTrace f
  --       <> "\nType: "
  --       <> ppTrace (fTy ^. typedType)
  --       <> "\nArgs: "
  --       <> ppTrace args
  --       <> "\nHint: "
  --       <> ppTrace mhint
  --   )
  st' <- execState iniBuilder goArgs
  let ret =
        TypedExpression
          { _typedType = st' ^. appBuilderType,
            _typedExpression = st' ^. appBuilder
          }
  -- traceM
  --   ( "\n\nReturn for HolesHelper\n"
  --       <> "Builder: "
  --       <> ppTrace f
  --       <> "\nType: "
  --       <> ppTrace (fTy ^. typedType)
  --       <> "\nArgs: "
  --       <> ppTrace args
  --       <> "\nRet: "
  --       <> ppTrace ret
  --   )
  return ret
  where
    goArgs :: forall r'. (r' ~ State AppBuilder ': r) => Sem r' ()
    goArgs = peekArg >>= maybe (insertTrailingHolesMay mhint) goNextArg
      where
        insertTrailingHolesMay :: Maybe Expression -> Sem r' ()
        insertTrailingHolesMay = flip whenJust insertTrailingHoles

        insertTrailingHoles :: Expression -> Sem r' ()
        insertTrailingHoles hintTy = do
          builderTy <- gets (^. appBuilderType)
          ariHint <- typeArity hintTy
          ariExpr <- typeArity builderTy
          let preImplicits :: Arity -> [IsImplicit]
              preImplicits = takeWhile isImplicitOrInstance . map (^. arityParameterImplicit) . unfoldArity
          let preAriExpr = preImplicits ariExpr
              preAriHint = preImplicits ariHint
          loc <- getLoc <$> gets (^. appBuilder)
          let toBeInserted :: [IsImplicit] = take (length preAriExpr - length preAriHint) preAriExpr
              mkHoleArg :: IsImplicit -> Sem r' ApplicationArg
              mkHoleArg i =
                ApplicationArg i <$> case i of
                  Explicit -> impossible
                  Implicit -> newHoleImplicit loc
                  ImplicitInstance -> newHoleInstance loc
          trailingHoles <- mapM mkHoleArg toBeInserted
          -- traceM ("adding trailing for " <> ppTrace builder
          --         <> "\nwith type: " <> ppTrace builderTy
          --         <> "\nariHint: " <> ppTrace ariHint
          --         <> "\nariExpr: " <> ppTrace ariExpr
          --         <> "\ntrailing: " <> ppTrace trailingHoles
          --         <> "\nmhint: " <> ppTrace hi'
          --        )
          mapM_ addTrailingHole trailingHoles
          where
            addTrailingHole :: ApplicationArg -> Sem r' ()
            addTrailingHole a = do
              -- traceM $ "insert trailing " <> ppTrace (a ^. appArgIsImplicit)
              fun <- peekFunctionType (a ^. appArgIsImplicit)
              modify' (over appBuilderArgs (a :))
              checkMatchingArg a fun

        checkMatchingArg :: ApplicationArg -> Function -> Sem r' ()
        checkMatchingArg arg fun = do
          -- traceM ("chekc arg: " <> ppTrace arg <> " " <> ppTrace (getLoc arg))
          dropArg
          let funParam = fun ^. functionLeft
              funL = funParam ^. paramType
              funR = fun ^. functionRight
          -- traceM ("checkMatching Arg " <> ppTrace arg <> " with " <> ppTrace funL)
          arg' <- checkExpression funL (arg ^. appArg)
          -- traceM ("AFTER checkMatching Arg " <> ppTrace arg <> " with " <> ppTrace funL
          --         <> " returns " <> ppTrace arg')
          let subs :: Expression -> Sem r' Expression = substitutionApp (funParam ^. paramName, arg')
              applyArg :: Expression -> Expression
              applyArg l =
                ExpressionApplication
                  Application
                    { _appLeft = l,
                      _appRight = arg',
                      _appImplicit = arg ^. appArgIsImplicit
                    }
          funR' <- subs funR
          modify' (set appBuilderType funR')
          modify' (over appBuilder applyArg)

        goNextArg :: ApplicationArg -> Sem r' ()
        goNextArg arg = do
          let i = arg ^. appArgIsImplicit
          fun <- peekFunctionType i
          insertMiddleHoleOrCheck fun i
          where
            insertMiddleHoleOrCheck :: Function -> IsImplicit -> Sem r' ()
            insertMiddleHoleOrCheck fun argImpl =
              let funParam = fun ^. functionLeft
                  funImpl = funParam ^. paramImplicit
                  checkThisArg = checkMatchingArg arg fun >> goArgs
               in case (argImpl, funImpl) of
                    (Explicit, Explicit) -> checkThisArg
                    (Implicit, Implicit) -> checkThisArg
                    (ImplicitInstance, ImplicitInstance) -> checkThisArg
                    (ImplicitInstance, Explicit) -> throwExpectedExplicit
                    (Implicit, Explicit) -> throwExpectedExplicit
                    (Explicit, Implicit) -> insertMiddleHole Implicit
                    (ImplicitInstance, Implicit) -> insertMiddleHole Implicit
                    (Implicit, ImplicitInstance) -> insertMiddleHole ImplicitInstance
                    (Explicit, ImplicitInstance) -> insertMiddleHole ImplicitInstance
              where
                insertMiddleHole :: IsImplicit -> Sem r' ()
                insertMiddleHole impl = do
                  l <- gets (^. appBuilder)
                  let loc = getLoc l
                  -- traceM $ "insert middle " <> ppTrace impl
                  h <- case impl of
                    Implicit -> newHoleImplicit loc
                    ImplicitInstance -> newHoleInstance loc
                    Explicit -> impossible
                  modify' (over appBuilderArgs (ApplicationArg impl h :))
                  goArgs

        throwExpectedExplicit :: Sem r' a
        throwExpectedExplicit = do
          l <- gets (^. appBuilder)
          throw
            . ErrArityCheckerError
            $ ErrExpectedExplicitArgument
              ExpectedExplicitArgument
                { _expectedExplicitArgumentApp = (l, error "FIXME"),
                  _expectedExplicitArgumentIx = error "FIXME"
                }

        peekFunctionType :: IsImplicit -> Sem r' Function
        peekFunctionType impl = do
          ty <- gets (^. appBuilderType) >>= weakNormalize
          case ty of
            ExpressionFunction f -> return f
            ExpressionHole h -> holeRefineToFunction impl h
            _ -> throwExpectedFunTy
              where
                throwExpectedFunTy :: Sem r' a
                throwExpectedFunTy = do
                  l <- gets (^. appBuilder)
                  builderTy <- gets (^. appBuilderType)
                  args <- gets (^. appBuilderArgs)
                  let a :: Expression = foldApplication l args
                  throw $
                    ErrExpectedFunctionType
                      ExpectedFunctionType
                        { _expectedFunctionTypeExpression = a,
                          _expectedFunctionTypeLeft = l,
                          _expectedFunctionTypeType = builderTy
                        }

        dropArg :: Sem r' ()
        dropArg = modify' (over appBuilderArgs (drop 1))

        peekArg :: Sem r' (Maybe ApplicationArg)
        peekArg = do
          b <- get
          return (head <$> nonEmpty (b ^. appBuilderArgs))

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
    _ -> throw (ErrImpracticalPatternMatching (ImpracticalPatternMatching ty))
  where
    viewTypeApp :: Expression -> (Expression, [Expression])
    viewTypeApp tyapp = case tyapp of
      ExpressionApplication (Application l r _) ->
        second (`snoc` r) (viewTypeApp l)
      _ -> (tyapp, [])

typeArity :: forall r. (Members '[Inference, Reader LocalVars] r) => Expression -> Sem r Arity
typeArity = weakNormalize >=> go
  where
    go :: Expression -> Sem r Arity
    go = \case
      ExpressionIden i -> goIden i
      ExpressionApplication a -> goApplication a
      ExpressionLiteral l -> goLiteral (l ^. withLocParam)
      ExpressionFunction f -> ArityFunction <$> goFun f
      ExpressionHole h -> return (ArityBlocking (BlockingHole h))
      ExpressionInstanceHole {} -> return ArityUnit
      ExpressionLambda {} -> return ArityError
      ExpressionCase {} -> return ArityNotKnown -- TODO Do better here
      ExpressionUniverse {} -> return ArityUnit
      ExpressionSimpleLambda {} -> simplelambda
      ExpressionLet l -> goLet l

    -- It will be a type error since there are no literals that are types at the moment
    goLiteral :: Literal -> Sem r Arity
    goLiteral _ = return ArityError

    -- TODO could we do better here?
    goApplication :: Application -> Sem r Arity
    goApplication a = case lhs of
      ExpressionIden IdenInductive {} -> return ArityUnit
      _ -> return ArityNotKnown
      where
        lhs :: Expression
        lhs = fst (unfoldApplication a)

    -- TODO use the type info in the let clauses
    goLet :: Let -> Sem r Arity
    goLet l = typeArity (l ^. letExpression)

    goIden :: Iden -> Sem r Arity
    goIden = \case
      IdenVar v -> return (ArityBlocking (BlockingVar v))
      IdenInductive {} -> return ArityUnit
      IdenFunction {} -> return ArityNotKnown
      IdenConstructor {} -> return ArityError
      IdenAxiom {} -> return ArityNotKnown

    goParam :: FunctionParameter -> Sem r ArityParameter
    goParam FunctionParameter {..} = do
      paramAri' <- case _paramImplicit of
        Explicit -> go _paramType
        Implicit -> go _paramType
        ImplicitInstance -> return ArityUnit
      return
        ArityParameter
          { _arityParameterArity = paramAri',
            _arityParameterImplicit = _paramImplicit,
            _arityParameterInfo = emptyArgInfo
          }

    goFun :: Function -> Sem r FunctionArity
    goFun (Function l r) = do
      l' <- goParam l
      r' <- go r
      return
        FunctionArity
          { _functionArityLeft = l',
            _functionArityRight = r'
          }

guessArity ::
  forall r.
  (Members '[Reader InfoTable, Inference] r) =>
  Expression ->
  Sem r Arity
guessArity = \case
  ExpressionHole {} -> return ArityNotKnown
  ExpressionInstanceHole {} -> return ArityUnit
  ExpressionFunction {} -> return ArityUnit
  ExpressionLiteral {} -> return arityLiteral
  ExpressionApplication a -> appHelper a
  ExpressionIden i -> idenHelper i
  ExpressionUniverse {} -> return arityUniverse
  ExpressionSimpleLambda {} -> simplelambda
  ExpressionLambda l -> arityLambda l
  ExpressionLet l -> arityLet l
  ExpressionCase l -> arityCase l
  where
    idenHelper :: Iden -> Sem r Arity
    idenHelper = withEmptyLocalVars . idenArity

    appHelper :: Application -> Sem r Arity
    appHelper a = do
      f' <- guessArity f
      let u = unfoldArity' f'
      return $ case refine args (u ^. ufoldArityParams) of
        Nothing -> ArityNotKnown
        Just a' -> foldArity (set ufoldArityParams a' u)
      where
        (f, args) = second (map (^. appArgIsImplicit) . toList) (unfoldApplication' a)

        refine :: [IsImplicit] -> [ArityParameter] -> Maybe [ArityParameter]
        refine as ps = case (as, ps) of
          (Explicit : as', ArityParameter {_arityParameterImplicit = Explicit} : ps') -> refine as' ps'
          (Implicit : as', ArityParameter {_arityParameterImplicit = Implicit} : ps') -> refine as' ps'
          (ImplicitInstance : as', ArityParameter {_arityParameterImplicit = ImplicitInstance} : ps') -> refine as' ps'
          (as'@(Explicit : _), ArityParameter {_arityParameterImplicit = Implicit} : ps') -> refine as' ps'
          (as'@(Explicit : _), ArityParameter {_arityParameterImplicit = ImplicitInstance} : ps') -> refine as' ps'
          (Implicit : _, ArityParameter {_arityParameterImplicit = Explicit} : _) -> Nothing
          (ImplicitInstance : _, ArityParameter {_arityParameterImplicit = Explicit} : _) -> Nothing
          (Implicit : _, ArityParameter {_arityParameterImplicit = ImplicitInstance} : _) -> Nothing
          (ImplicitInstance : _, ArityParameter {_arityParameterImplicit = Implicit} : _) -> Nothing
          ([], ps') -> Just ps'
          (_ : _, []) -> Nothing

arityLiteral :: Arity
arityLiteral = ArityUnit

arityUniverse :: Arity
arityUniverse = ArityUnit

simplelambda :: a
simplelambda = error "simple lambda expressions are not supported by the arity checker"

arityLambda :: forall r. (Members '[Reader InfoTable, Inference] r) => Lambda -> Sem r Arity
arityLambda l = do
  aris <- mapM guessClauseArity (l ^. lambdaClauses)
  return $
    if
        | allSame aris -> head aris
        | otherwise -> ArityNotKnown
  where
    guessClauseArity :: LambdaClause -> Sem r Arity
    guessClauseArity cl = do
      body <- unfoldArity' <$> guessArity (cl ^. lambdaBody)
      let ps = guessPatternArgArity <$> cl ^. lambdaPatterns
          uari' =
            UnfoldedArity
              { _ufoldArityParams = toList ps <> body ^. ufoldArityParams,
                _ufoldArityRest = body ^. ufoldArityRest
              }
      return (foldArity uari')

guessPatternArity :: Pattern -> Arity
guessPatternArity = \case
  PatternVariable {} -> ArityNotKnown
  PatternWildcardConstructor {} -> ArityUnit
  PatternConstructorApp {} -> ArityUnit

guessPatternArgArity :: PatternArg -> ArityParameter
guessPatternArgArity p =
  ArityParameter
    { _arityParameterArity = guessPatternArity (p ^. patternArgPattern),
      _arityParameterImplicit = p ^. patternArgIsImplicit,
      _arityParameterInfo =
        ArgInfo
          { _argInfoDefault = Nothing,
            _argInfoName = Nothing
          }
    }

arityLet :: (Members '[Reader InfoTable, Inference] r) => Let -> Sem r Arity
arityLet l = guessArity (l ^. letExpression)

-- | All branches should have the same arity. If they are all the same, we
-- return that, otherwise we return ArityBlocking. Probably something better can
-- be done.
arityCase :: (Members '[Reader InfoTable, Inference] r) => Case -> Sem r Arity
arityCase c = do
  aris <- mapM (guessArity . (^. caseBranchExpression)) (c ^. caseBranches)
  return
    if
        | allSame aris -> head aris
        | otherwise -> ArityNotKnown

idenArity :: (Members '[Inference, Reader LocalVars, Reader InfoTable] r) => Iden -> Sem r Arity
idenArity = \case
  IdenVar v -> getLocalArity v
  IdenInductive i -> lookupInductiveType i >>= typeArity
  IdenFunction f -> do
    fun <- (^. functionInfoDef) <$> lookupFunction f
    ari <- typeArity (fun ^. funDefType)
    let defaults = fun ^. funDefArgsInfo
    return (addArgsInfo defaults ari)
  IdenConstructor c -> lookupConstructorType c >>= typeArity
  IdenAxiom a -> lookupAxiom a >>= typeArity . (^. axiomInfoDef . axiomType)

addArgsInfo :: [ArgInfo] -> Arity -> Arity
addArgsInfo = unfoldingArity . helper
  where
    helper :: [ArgInfo] -> UnfoldedArity -> UnfoldedArity
    helper = over ufoldArityParams . go

    go :: [ArgInfo] -> [ArityParameter] -> [ArityParameter]
    go infos params = case infos of
      [] -> params
      info : infos' -> case params of
        [] -> impossible
        para : params' ->
          set arityParameterInfo info para : go infos' params'

getLocalArity :: (Members '[Reader LocalVars, Inference] r) => VarName -> Sem r Arity
getLocalArity v = do
  mty <- asks (^. localTypes . at v)
  maybe (return ArityNotKnown) typeArity mty

newHoleImplicit :: (Member NameIdGen r) => Interval -> Sem r Expression
newHoleImplicit loc = ExpressionHole . mkHole loc <$> freshNameId

newHoleInstance :: (Member NameIdGen r) => Interval -> Sem r Expression
newHoleInstance loc = ExpressionInstanceHole . mkInstanceHole loc <$> freshNameId
