module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.FunctionsTable,
    Inference,
    MatchError,
    registerFunctionDef,
    matchErrorLeft,
    matchErrorRight,
    queryMetavar,
    registerIdenType,
    strongNormalize',
    iniState,
    strongNormalize,
    weakNormalize,
    runInferenceDefs,
    runInferenceDef,
    rememberFunctionDef,
    matchTypes,
    queryMetavarFinal,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.FunctionsTable
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude hiding (fromEither)

data MetavarState
  = Fresh
  | -- | Type may contain holes
    Refined Expression

data MatchError = MatchError
  { _matchErrorLeft :: Expression,
    _matchErrorRight :: Expression
  }

makeLenses ''MatchError

data Inference m a where
  MatchTypes :: Expression -> Expression -> Inference m (Maybe MatchError)
  QueryMetavar :: Hole -> Inference m (Maybe Expression)
  RegisterIdenType :: Name -> Expression -> Inference m ()
  RememberFunctionDef :: FunctionDef -> Inference m ()
  StrongNormalize :: Expression -> Inference m Expression
  WeakNormalize :: Expression -> Inference m Expression

makeSem ''Inference

data InferenceState = InferenceState
  { _inferenceMap :: HashMap Hole MetavarState,
    _inferenceFunctionsStash :: [FunctionDef],
    _inferenceIdens :: TypesTable
  }

makeLenses ''InferenceState

iniState :: InferenceState
iniState =
  InferenceState
    { _inferenceMap = mempty,
      _inferenceFunctionsStash = [],
      _inferenceIdens = mempty
    }

closeState ::
  (Member (Error TypeCheckerError) r) =>
  InferenceState ->
  Sem
    r
    ( HashMap Hole Expression,
      TypesTable
    )
closeState = \case
  InferenceState {..} -> do
    holeMap <- execState mempty (f _inferenceMap)
    return (holeMap, _inferenceIdens)
  where
    f ::
      forall r'.
      (Members '[Error TypeCheckerError, State (HashMap Hole Expression)] r') =>
      HashMap Hole MetavarState ->
      Sem r' ()
    f m = mapM_ goHole (HashMap.keys m)
      where
        getState :: Hole -> MetavarState
        getState h = fromMaybe err (m ^. at h)
          where
            err :: a
            err = error ("Impossible: could not find the state for the hole " <> ppTrace h)
        goHole :: Hole -> Sem r' Expression
        goHole h =
          let st = getState h
           in case st of
                Fresh ->
                  throw $
                    ErrUnsolvedMeta
                      UnsolvedMeta
                        { _unsolvedMeta = h,
                          _unsolvedIsLoop = False
                        }
                Refined t -> do
                  s <- gets @(HashMap Hole Expression) (^. at h)
                  case s of
                    Just noHolesTy -> return noHolesTy
                    Nothing -> do
                      x <- goExpression t
                      modify (HashMap.insert h x)
                      return x
        goExpression :: Expression -> Sem r' Expression
        goExpression = traverseOf leafExpressions aux
          where
            aux :: Expression -> Sem r' Expression
            aux = \case
              ExpressionHole h -> goHole h
              e -> return e

getMetavar :: (Member (State InferenceState) r) => Hole -> Sem r MetavarState
getMetavar h = do
  void (queryMetavar' h)
  gets (fromJust . (^. inferenceMap . at h))

queryMetavarFinal :: (Member Inference r) => Hole -> Sem r (Maybe Expression)
queryMetavarFinal h = do
  m <- queryMetavar h
  case m of
    Just (ExpressionHole h') -> queryMetavarFinal h'
    _ -> return m

strongNormalize' :: forall r. (Members '[State FunctionsTable, State InferenceState, NameIdGen] r) => Expression -> Sem r Expression
strongNormalize' = go
  where
    go :: Expression -> Sem r Expression
    go e = case e of
      ExpressionIden i -> goIden i
      ExpressionApplication app -> goApp app
      ExpressionLiteral {} -> return e
      ExpressionHole h -> goHole h
      ExpressionInstanceHole h -> goInstanceHole h
      ExpressionUniverse {} -> return e
      ExpressionFunction f -> ExpressionFunction <$> goFunction f
      ExpressionSimpleLambda l -> ExpressionSimpleLambda <$> goSimpleLambda l
      ExpressionLambda l -> ExpressionLambda <$> goLambda l
      -- NOTE: we cannot always normalize let expressions because we do not have closures.
      -- We just recurse inside
      ExpressionLet l -> ExpressionLet <$> goLet l
      -- TODO it should normalize like an applied lambda
      ExpressionCase {} -> error "normalization of case expressions is not supported yet"

    goLet :: Let -> Sem r Let
    goLet Let {..} = do
      clauses' <- mapM goLetClause _letClauses
      body' <- go _letExpression
      return
        Let
          { _letClauses = clauses',
            _letExpression = body'
          }
      where
        goLetClause :: LetClause -> Sem r LetClause
        goLetClause = \case
          LetFunDef f -> LetFunDef <$> goFunDef f
          LetMutualBlock f -> LetMutualBlock <$> goMutualBlockLet f
          where
            goMutualBlockLet :: MutualBlockLet -> Sem r MutualBlockLet
            goMutualBlockLet MutualBlockLet {..} = do
              defs' <- mapM goFunDef _mutualLet
              return MutualBlockLet {_mutualLet = defs'}

            goFunDef :: FunctionDef -> Sem r FunctionDef
            goFunDef FunctionDef {..} = do
              type' <- go _funDefType
              body' <- go _funDefBody
              return
                FunctionDef
                  { _funDefBody = body',
                    _funDefType = type',
                    ..
                  }

    goLambda :: Lambda -> Sem r Lambda
    goLambda l = do
      _lambdaClauses <- mapM goLambdaClause (l ^. lambdaClauses)
      _lambdaType <- mapM go (l ^. lambdaType)
      return Lambda {..}
      where
        goLambdaClause :: LambdaClause -> Sem r LambdaClause
        goLambdaClause (LambdaClause p b) = do
          b' <- go b
          return (LambdaClause p b')

    goSimpleLambda :: SimpleLambda -> Sem r SimpleLambda
    goSimpleLambda (SimpleLambda (SimpleBinder lamVar lamTy) lamBody) = do
      lamTy' <- go lamTy
      lamBody' <- go lamBody
      return (SimpleLambda (SimpleBinder lamVar lamTy') lamBody')

    goFunctionParam :: FunctionParameter -> Sem r FunctionParameter
    goFunctionParam (FunctionParameter mvar i ty) = do
      ty' <- go ty
      return (FunctionParameter mvar i ty')

    goFunction :: Function -> Sem r Function
    goFunction (Function l r) = do
      l' <- goFunctionParam l
      r' <- go r
      return (Function l' r')

    goHole :: Hole -> Sem r Expression
    goHole h = do
      s <- getMetavar h
      case s of
        Fresh -> return (ExpressionHole h)
        Refined r -> go r

    goInstanceHole :: InstanceHole -> Sem r Expression
    goInstanceHole = return . ExpressionInstanceHole

    goApp :: Application -> Sem r Expression
    goApp (Application l r i) = do
      l' <- go l
      case l' of
        ExpressionSimpleLambda (SimpleLambda (SimpleBinder lamVar _) lamBody) -> do
          b' <- substitutionE (HashMap.singleton lamVar r) lamBody
          go b'
        _ -> do
          r' <- go r
          return (ExpressionApplication (Application l' r' i))

    goIden :: Iden -> Sem r Expression
    goIden i = case i of
      IdenFunction f -> do
        f' <- askFunctionDef f
        case f' of
          Nothing -> return i'
          Just x -> go x
      _ -> return i'
      where
        i' = ExpressionIden i

weakNormalize' :: forall r. (Members '[State FunctionsTable, State InferenceState, NameIdGen] r) => Expression -> Sem r Expression
weakNormalize' = go
  where
    go :: Expression -> Sem r Expression
    go e = case e of
      ExpressionIden i -> goIden i
      ExpressionHole h -> goHole h
      ExpressionInstanceHole h -> goInstanceHole h
      ExpressionApplication a -> goApp a
      ExpressionLiteral {} -> return e
      ExpressionUniverse {} -> return e
      ExpressionFunction {} -> return e
      ExpressionSimpleLambda {} -> return e
      ExpressionLambda {} -> return e
      ExpressionLet {} -> return e
      ExpressionCase {} -> return e
    goIden :: Iden -> Sem r Expression
    goIden i = case i of
      IdenFunction f -> do
        f' <- askFunctionDef f
        case f' of
          Nothing -> return i'
          Just x -> go x
      _ -> return i'
      where
        i' = ExpressionIden i
    goApp :: Application -> Sem r Expression
    goApp (Application l r i) = do
      l' <- go l
      case l' of
        ExpressionSimpleLambda (SimpleLambda (SimpleBinder lamVar _) lamBody) -> do
          b' <- substitutionE (HashMap.singleton lamVar r) lamBody
          go b'
        _ -> return (ExpressionApplication (Application l' r i))
    goHole :: Hole -> Sem r Expression
    goHole h = do
      s <- getMetavar h
      case s of
        Fresh -> return (ExpressionHole h)
        Refined r -> go r
    goInstanceHole :: InstanceHole -> Sem r Expression
    goInstanceHole = return . ExpressionInstanceHole

queryMetavar' :: (Members '[State InferenceState] r) => Hole -> Sem r (Maybe Expression)
queryMetavar' h = do
  m <- gets (^. inferenceMap . at h)
  case m of
    Nothing -> do
      modify (over inferenceMap (HashMap.insert h Fresh))
      return Nothing
    Just Fresh -> return Nothing
    Just (Refined e) -> return (Just e)

data MatchCtx = MatchCtx
  { _matchAlpha :: Rename,
    _matchLeftAbsorb :: Rename,
    _matchRightAbsorb :: Rename
  }

makeLenses ''MatchCtx

re ::
  (Members '[State FunctionsTable, Error TypeCheckerError, NameIdGen] r) =>
  Sem (Inference ': r) a ->
  Sem (State InferenceState ': r) a
re = reinterpret $ \case
  MatchTypes a b -> matchTypes' a b
  QueryMetavar h -> queryMetavar' h
  RememberFunctionDef f -> modify' (over inferenceFunctionsStash (f :))
  RegisterIdenType i ty -> registerIdenType' i ty
  StrongNormalize ty -> strongNormalize' ty
  WeakNormalize ty -> weakNormalize' ty
  where
    registerIdenType' :: (Members '[State InferenceState] r) => Name -> Expression -> Sem r ()
    registerIdenType' i ty = modify (over inferenceIdens (HashMap.insert (i ^. nameId) ty))

    -- Supports alpha equivalence.
    matchTypes' :: (Members '[State InferenceState, State FunctionsTable, Error TypeCheckerError, NameIdGen] r) => Expression -> Expression -> Sem r (Maybe MatchError)
    matchTypes' ty = fmap (^? _Left) . runError . matchTypesNoCtx ty
      where
        matchTypesNoCtx :: (Members '[State InferenceState, State FunctionsTable, Error TypeCheckerError, NameIdGen, Error MatchError] r) => Expression -> Expression -> Sem r ()
        matchTypesNoCtx nty = runReader ini . go nty
        ini :: MatchCtx
        ini =
          MatchCtx
            { _matchAlpha = mempty,
              _matchLeftAbsorb = mempty,
              _matchRightAbsorb = mempty
            }
        go ::
          forall r.
          (Members '[State InferenceState, Reader MatchCtx, State FunctionsTable, Error TypeCheckerError, NameIdGen, Error MatchError] r) =>
          Expression ->
          Expression ->
          Sem r ()
        go inputA inputB = do
          normA <- weakNormalize' inputA
          normB <- weakNormalize' inputB
          goNormalized normA normB
          where
            goNormalized :: Expression -> Expression -> Sem r ()
            goNormalized normA normB =
              case (normA, normB) of
                (ExpressionIden a, ExpressionIden b) -> goIden a b
                (ExpressionApplication a, ExpressionApplication b) -> goApplication a b
                (ExpressionFunction a, ExpressionFunction b) -> goFunction a b
                (ExpressionUniverse u, ExpressionUniverse u') -> check (u == u')
                (ExpressionSimpleLambda a, ExpressionSimpleLambda b) -> goSimpleLambda a b
                (ExpressionLambda a, ExpressionLambda b) -> goLambda a b
                (ExpressionHole h, a) -> goHole h a
                (a, ExpressionHole h) -> goHole h a
                (_, ExpressionLet r) -> go normA (r ^. letExpression)
                (ExpressionLet l, _) -> go (l ^. letExpression) normB
                (ExpressionInstanceHole {}, _) -> err
                (_, ExpressionInstanceHole {}) -> err
                (ExpressionSimpleLambda {}, _) -> err
                (_, ExpressionSimpleLambda {}) -> err
                (ExpressionIden {}, _) -> err
                (_, ExpressionIden {}) -> err
                (ExpressionApplication {}, _) -> err
                (_, ExpressionApplication {}) -> err
                (ExpressionFunction {}, _) -> err
                (_, ExpressionFunction {}) -> err
                (ExpressionUniverse {}, _) -> err
                (_, ExpressionUniverse {}) -> err
                (ExpressionLambda {}, _) -> err
                (_, ExpressionLambda {}) -> err
                (_, ExpressionCase {}) -> error "not implemented"
                (ExpressionCase {}, _) -> error "not implemented"
                (ExpressionLiteral l, ExpressionLiteral l') -> check (l == l')
              where
                ok :: Sem r ()
                ok = return ()

                check :: Bool -> Sem r ()
                check b
                  | b = ok
                  | otherwise = err

                bicheck :: Sem r () -> Sem r () -> Sem r ()
                bicheck = (>>)

                err :: Sem r ()
                err = throw (MatchError normA normB)

                goHole :: Hole -> Expression -> Sem r ()
                goHole h t = do
                  r <- queryMetavar' h
                  case r of
                    Nothing -> refineFreshMetavar h t
                    -- Just ht -> matchTypes' t ht
                    -- Just ht -> matchTypesNoCtx t ht
                    Just ht -> go t ht
                  where
                    refineFreshMetavar :: Hole -> Expression -> Sem r ()
                    refineFreshMetavar hol holTy
                      | ExpressionHole h' <- holTy, h' == hol = return ()
                      | otherwise =
                          do
                            holTy' <- strongNormalize' holTy
                            let er =
                                  ErrUnsolvedMeta
                                    UnsolvedMeta
                                      { _unsolvedMeta = hol,
                                        _unsolvedIsLoop = True
                                      }
                            when (ExpressionHole hol `elem` holTy' ^.. leafExpressions) (throw er)
                            s <- gets (fromJust . (^. inferenceMap . at hol))
                            case s of
                              Fresh -> modify (over inferenceMap (HashMap.insert hol (Refined holTy')))
                              Refined {} -> impossible

                goIden :: Iden -> Iden -> Sem r ()
                goIden ia ib = case (ia, ib) of
                  (IdenInductive a, IdenInductive b) -> check (a == b)
                  (IdenAxiom a, IdenAxiom b) -> check (a == b)
                  (IdenFunction a, IdenFunction b) -> check (a == b)
                  (IdenConstructor a, IdenConstructor b) -> check (a == b)
                  (IdenVar a, IdenVar b) -> do
                    mappedEq <- (== Just b) . HashMap.lookup a <$> asks (^. matchAlpha)
                    check (a == b || mappedEq)
                  (IdenAxiom {}, _) -> err
                  (_, IdenAxiom {}) -> err
                  (IdenFunction {}, _) -> err
                  (_, IdenFunction {}) -> err
                  (_, IdenVar {}) -> err
                  (IdenVar {}, _) -> err
                  (IdenConstructor {}, _) -> err
                  (_, IdenConstructor {}) -> err

                goApplication :: Application -> Application -> Sem r ()
                goApplication (Application f x _) (Application f' x' _) = bicheck (go f f') (go x x')

                goSimpleLambda :: SimpleLambda -> SimpleLambda -> Sem r ()
                goSimpleLambda (SimpleLambda (SimpleBinder v1 ty1) b1) (SimpleLambda (SimpleBinder v2 ty2) b2) = do
                  let local' :: Sem r x -> Sem r x
                      local' = local (over matchAlpha (HashMap.insert v1 v2))
                  bicheck (go ty1 ty2) (local' (go b1 b2))

                goFunction :: Function -> Function -> Sem r ()
                goFunction
                  (Function (FunctionParameter mname1 i1 l1) r1)
                  (Function (FunctionParameter mname2 i2 l2) r2) = do
                    unless (i1 == i2) err
                    l1' <- weakNormalize' l1
                    l2' <- weakNormalize' l1
                    let adjustAlpha :: MatchCtx -> MatchCtx
                        adjustAlpha = case (mname1, mname2) of
                          (Just var1, Just var2) -> over matchAlpha (HashMap.insert var1 var2)
                          _ -> id

                        adjustAbsorb :: MatchCtx -> MatchCtx
                        adjustAbsorb ctx = fromMaybe ctx $ do
                          var1 <- mname1
                          var2 <- mname2

                          traceM "absorbing 00"
                          let holeInLeft = do
                                guard (isNormalHole l1')
                                return (over matchLeftAbsorb (HashMap.insert var2 var1) ctx)

                              holeInRight = do
                                guard (isNormalHole l2')
                                return (over matchRightAbsorb (HashMap.insert var1 var2) ctx)

                          res <- holeInLeft <|> holeInRight
                          traceM "absorbing!!!"
                          return res

                        withCtx' :: Sem r x -> Sem r x
                        withCtx' = local (adjustAbsorb . adjustAlpha)
                    bicheck (go l1 l2) (withCtx' (go r1 r2))

                -- NOTE type is ignored, I think it is ok
                goLambda :: Lambda -> Lambda -> Sem r ()
                goLambda (Lambda l1 _) (Lambda l2 _) = case zipExactMay (toList l1) (toList l2) of
                  Just z -> mapM_ (uncurry goClause) z
                  _ -> err
                  where
                    goClause :: LambdaClause -> LambdaClause -> Sem r ()
                    goClause (LambdaClause p1 b1) (LambdaClause p2 b2) =
                      case zipExactMay (toList p1) (toList p2) of
                        Nothing -> err
                        Just z -> do
                          m <- asks (^. matchAlpha)
                          (m', patMatch) <- runState m (mapM (uncurry matchPatterns) z)
                          if
                              | and patMatch -> local (set matchAlpha m') (go b1 b2)
                              | otherwise -> err

matchPatterns ::
  forall r.
  (Members '[State InferenceState, State (HashMap VarName VarName), State FunctionsTable] r) =>
  PatternArg ->
  PatternArg ->
  Sem r Bool
matchPatterns (PatternArg impl1 name1 pat1) (PatternArg impl2 name2 pat2) =
  return (impl1 == impl2) &&>= goName name1 name2 &&>= goPattern pat1 pat2
  where
    (&&>=) :: (Monad m) => m Bool -> m Bool -> m Bool
    (&&>=) = liftM2 (&&)
    goName :: Maybe VarName -> Maybe VarName -> Sem r Bool
    goName (Just n1) (Just n2) = modify (HashMap.insert n1 n2) $> True
    goName Nothing Nothing = ok
    goName _ _ = err
    goPattern :: Pattern -> Pattern -> Sem r Bool
    goPattern p1 p2 = case (p1, p2) of
      (PatternVariable v1, PatternVariable v2) -> modify (HashMap.insert v1 v2) $> True
      (PatternConstructorApp c1, PatternConstructorApp c2) -> goConstructor c1 c2
      (PatternVariable {}, _) -> err
      (_, PatternVariable {}) -> err
      (PatternWildcardConstructor {}, _) -> impossible
      (_, PatternWildcardConstructor {}) -> impossible
    goConstructor :: ConstructorApp -> ConstructorApp -> Sem r Bool
    goConstructor (ConstructorApp c1 args1 _) (ConstructorApp c2 args2 _)
      | c1 /= c2 = err
      | otherwise = case zipExactMay args1 args2 of
          Nothing -> err
          Just z -> allM (uncurry matchPatterns) z

    ok :: Sem r Bool
    ok = return True
    err :: Sem r Bool
    err = return False

runInferenceDefs ::
  (Members '[Termination, HighlightBuilder, Error TypeCheckerError, State FunctionsTable, State TypesTable, NameIdGen] r, HasExpressions funDef) =>
  Sem (Inference ': r) (NonEmpty funDef) ->
  Sem r (NonEmpty funDef)
runInferenceDefs a = do
  (finalState, expr) <- runState iniState (re a)
  (subs, idens) <- closeState finalState
  idens' <- mapM (subsHoles subs) idens
  stash' <- mapM (subsHoles subs) (finalState ^. inferenceFunctionsStash)
  forM_ stash' registerFunctionDef
  addIdens idens'
  mapM (subsHoles subs) expr

runInferenceDef ::
  (Members '[Termination, HighlightBuilder, Error TypeCheckerError, State FunctionsTable, State TypesTable, NameIdGen] r, HasExpressions funDef) =>
  Sem (Inference ': r) funDef ->
  Sem r funDef
runInferenceDef = fmap head . runInferenceDefs . fmap pure

addIdens :: (Members '[HighlightBuilder, State TypesTable] r) => TypesTable -> Sem r ()
addIdens idens = do
  modify (HashMap.union idens)
  modify (over highlightTypes (HashMap.union idens))

-- | Assumes the given function has been type checked. Does *not* register the
-- function.
-- Conditons:
-- 1. Only one clause.
-- 2. No pattern matching.
-- 3. Terminates.
--
-- Throws an error if the return type is Type and it does not satisfy the
-- above conditions.
functionDefEval :: forall r'. (Members '[State FunctionsTable, Termination, Error TypeCheckerError, NameIdGen] r') => FunctionDef -> Sem r' (Maybe Expression)
functionDefEval f = do
  (params :: [FunctionParameter], ret :: Expression) <- unfoldFunType <$> strongNorm (f ^. funDefType)
  r <- runFail (goTop params (canBeUniverse ret))
  when (isNothing r && isUniverse ret) (throw (ErrUnsupportedTypeFunction (UnsupportedTypeFunction f)))
  return r
  where
    strongNorm :: (Members '[State FunctionsTable, NameIdGen] r) => Expression -> Sem r Expression
    strongNorm = evalState iniState . strongNormalize'

    isUniverse :: Expression -> Bool
    isUniverse = \case
      ExpressionUniverse {} -> True
      _ -> False

    canBeUniverse :: Expression -> Bool
    canBeUniverse = \case
      ExpressionUniverse {} -> True
      ExpressionHole {} -> True
      ExpressionIden {} -> True
      _ -> False

    goTop ::
      forall r.
      (Members '[Fail, State FunctionsTable, Error TypeCheckerError, Termination] r) =>
      [FunctionParameter] ->
      Bool ->
      Sem r Expression
    goTop params returnsType = do
      checkTerminating
      goBody (f ^. funDefBody)
      where
        checkTerminating :: Sem r ()
        checkTerminating = unlessM (functionSafeToNormalize (f ^. funDefName)) fail

        goBody :: Expression -> Sem r Expression
        goBody body = do
          checkOneClause
          patsTys <- splitExplicitParams
          go (zipExact pats patsTys)
          where
            (pats, body') = unfoldLambda body
            checkOneClause = case body of
              ExpressionLambda Lambda {_lambdaClauses = _ :| _ : _} -> fail
              _ -> return ()
            splitExplicitParams :: Sem r [Expression]
            splitExplicitParams = do
              let n = length pats
              unless returnsType fail
              nfirst <- failMaybe (takeExactMay n params)
              mapM simpleExplicitParam nfirst
            simpleExplicitParam :: FunctionParameter -> Sem r Expression
            simpleExplicitParam = \case
              FunctionParameter _ Explicit ty -> return ty
              _ -> fail
            goPattern :: (Pattern, Expression) -> Expression -> Sem r Expression
            goPattern (p, ty) = case p of
              PatternVariable v -> return . ExpressionSimpleLambda . SimpleLambda (SimpleBinder v ty)
              _ -> const fail
            go :: [(PatternArg, Expression)] -> Sem r Expression
            go = \case
              [] -> return body'
              (p, ty) : ps
                | isImplicitOrInstance (p ^. patternArgIsImplicit) -> fail
                | otherwise -> go ps >>= goPattern (p ^. patternArgPattern, ty)

registerFunctionDef :: (Members '[State FunctionsTable, Error TypeCheckerError, NameIdGen, Termination] r) => FunctionDef -> Sem r ()
registerFunctionDef f = whenJustM (functionDefEval f) $ \e ->
  modify (over functionsTable (HashMap.insert (f ^. funDefName) e))
