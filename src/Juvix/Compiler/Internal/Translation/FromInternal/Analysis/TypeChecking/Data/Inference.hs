module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.FunctionsTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
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
  RegisterIden :: Name -> Expression -> Inference m ()
  StrongNormalize :: Expression -> Inference m Expression
  WeakNormalize :: Expression -> Inference m Expression

makeSem ''Inference

data InferenceState = InferenceState
  { _inferenceMap :: HashMap Hole MetavarState,
    _inferenceIdens :: TypesTable
  }

makeLenses ''InferenceState

iniState :: InferenceState
iniState = InferenceState mempty mempty

closeState ::
  (Member (Error TypeCheckerError) r) =>
  InferenceState ->
  Sem
    r
    ( HashMap Hole Expression,
      TypesTable
    )
closeState = \case
  InferenceState m idens -> do
    holeMap <- execState mempty (f m)
    return (holeMap, idens)
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
                Fresh -> throw (ErrUnsolvedMeta (UnsolvedMeta h))
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

strongNormalize' :: forall r. (Members '[Reader FunctionsTable, State InferenceState] r) => Expression -> Sem r Expression
strongNormalize' = go
  where
    go :: Expression -> Sem r Expression
    go e = case e of
      ExpressionIden i -> goIden i
      ExpressionApplication app -> goApp app
      ExpressionLiteral {} -> return e
      ExpressionHole h -> goHole h
      ExpressionUniverse {} -> return e
      ExpressionFunction f -> ExpressionFunction <$> goFunction f
      ExpressionSimpleLambda l -> ExpressionSimpleLambda <$> goSimpleLambda l
      ExpressionLambda l -> ExpressionLambda <$> goLambda l
      -- NOTE: we cannot always normalize let expressions because we do not have closures.
      -- We give up
      ExpressionLet {} -> error "normalization of let expressions is not supported yet"

    goClause :: LambdaClause -> Sem r LambdaClause
    goClause (LambdaClause p b) = do
      b' <- go b
      return (LambdaClause p b')

    goLambda :: Lambda -> Sem r Lambda
    goLambda (Lambda cl) = Lambda <$> mapM goClause cl

    goSimpleLambda :: SimpleLambda -> Sem r SimpleLambda
    goSimpleLambda (SimpleLambda lamVar lamTy lamBody) = do
      lamTy' <- go lamTy
      lamBody' <- go lamBody
      return (SimpleLambda lamVar lamTy' lamBody')

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

    goApp :: Application -> Sem r Expression
    goApp (Application l r i) = do
      l' <- go l
      case l' of
        ExpressionSimpleLambda (SimpleLambda lamVar _ lamBody) -> do
          go (substitutionE (HashMap.singleton lamVar r) lamBody)
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

weakNormalize' :: forall r. (Members '[Reader FunctionsTable, State InferenceState] r) => Expression -> Sem r Expression
weakNormalize' = go
  where
    go :: Expression -> Sem r Expression
    go e = case e of
      ExpressionIden i -> goIden i
      ExpressionHole h -> goHole h
      ExpressionApplication a -> goApp a
      ExpressionLiteral {} -> return e
      ExpressionUniverse {} -> return e
      ExpressionFunction {} -> return e
      ExpressionSimpleLambda {} -> return e
      ExpressionLambda {} -> return e
      ExpressionLet {} -> return e
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
        ExpressionSimpleLambda (SimpleLambda lamVar _ lamBody) -> do
          go (substitutionE (HashMap.singleton lamVar r) lamBody)
        _ -> return (ExpressionApplication (Application l' r i))
    goHole :: Hole -> Sem r Expression
    goHole h = do
      s <- getMetavar h
      case s of
        Fresh -> return (ExpressionHole h)
        Refined r -> go r

queryMetavar' :: (Members '[State InferenceState] r) => Hole -> Sem r (Maybe Expression)
queryMetavar' h = do
  m <- gets (^. inferenceMap . at h)
  case m of
    Nothing -> do
      modify (over inferenceMap (HashMap.insert h Fresh))
      return Nothing
    Just Fresh -> return Nothing
    Just (Refined e) -> return (Just e)

re ::
  (Members '[Reader FunctionsTable, Error TypeCheckerError] r) =>
  Sem (Inference ': r) a ->
  Sem (State InferenceState ': r) a
re = reinterpret $ \case
  MatchTypes a b -> matchTypes' a b
  QueryMetavar h -> queryMetavar' h
  RegisterIden i ty -> registerIden' i ty
  StrongNormalize ty -> strongNormalize' ty
  WeakNormalize ty -> weakNormalize' ty
  where
    registerIden' :: (Members '[State InferenceState] r) => Name -> Expression -> Sem r ()
    registerIden' i ty = modify (over inferenceIdens (HashMap.insert i ty))

    -- Supports alpha equivalence.
    matchTypes' :: (Members '[State InferenceState, Reader FunctionsTable, Error TypeCheckerError] r) => Expression -> Expression -> Sem r (Maybe MatchError)
    matchTypes' ty = runReader ini . go ty
      where
        ini :: HashMap VarName VarName
        ini = mempty
        go ::
          forall r.
          (Members '[State InferenceState, Reader (HashMap VarName VarName), Reader FunctionsTable, Error TypeCheckerError] r) =>
          Expression ->
          Expression ->
          Sem r (Maybe MatchError)
        go inputA inputB = do
          normA <- weakNormalize' inputA
          normB <- weakNormalize' inputB
          goNormalized normA normB
          where
            goNormalized :: Expression -> Expression -> Sem r (Maybe MatchError)
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
                (_, ExpressionLet {}) -> error "not implemented"
                (ExpressionLet {}, _) -> error "not implemented"
                (ExpressionLiteral l, ExpressionLiteral l') -> check (l == l')
              where
                ok :: Sem r (Maybe MatchError)
                ok = return Nothing
                check :: Bool -> Sem r (Maybe MatchError)
                check b
                  | b = ok
                  | otherwise = err
                bicheck :: Sem r (Maybe MatchError) -> Sem r (Maybe MatchError) -> Sem r (Maybe MatchError)
                bicheck = liftA2 (<|>)
                err :: Sem r (Maybe MatchError)
                err = return (Just (MatchError normA normB))
                goHole :: Hole -> Expression -> Sem r (Maybe MatchError)
                goHole h t = do
                  r <- queryMetavar' h
                  case r of
                    Nothing -> refineFreshMetavar h t $> Nothing
                    Just ht -> matchTypes' t ht
                  where
                    refineFreshMetavar :: Hole -> Expression -> Sem r ()
                    refineFreshMetavar hol holTy
                      | ExpressionHole h' <- holTy, h' == hol = return ()
                      | otherwise =
                          do
                            holTy' <- strongNormalize' holTy
                            when (ExpressionHole hol `elem` holTy' ^.. leafExpressions) (throw (ErrUnsolvedMeta (UnsolvedMeta hol)))
                            s <- gets (fromJust . (^. inferenceMap . at hol))
                            case s of
                              Fresh -> modify (over inferenceMap (HashMap.insert hol (Refined holTy')))
                              Refined {} -> impossible

                goIden :: Iden -> Iden -> Sem r (Maybe MatchError)
                goIden ia ib = case (ia, ib) of
                  (IdenInductive a, IdenInductive b) -> check (a == b)
                  (IdenAxiom a, IdenAxiom b) -> check (a == b)
                  (IdenFunction a, IdenFunction b) -> check (a == b)
                  (IdenConstructor a, IdenConstructor b) -> check (a == b)
                  (IdenVar a, IdenVar b) -> do
                    mappedEq <- (== Just b) . HashMap.lookup a <$> ask
                    check (a == b || mappedEq)
                  (IdenAxiom {}, _) -> err
                  (_, IdenAxiom {}) -> err
                  (IdenFunction {}, _) -> err
                  (_, IdenFunction {}) -> err
                  (_, IdenVar {}) -> err
                  (IdenVar {}, _) -> err
                  (IdenConstructor {}, _) -> err
                  (_, IdenConstructor {}) -> err

                goApplication :: Application -> Application -> Sem r (Maybe MatchError)
                goApplication (Application f x _) (Application f' x' _) = bicheck (go f f') (go x x')

                goSimpleLambda :: SimpleLambda -> SimpleLambda -> Sem r (Maybe MatchError)
                goSimpleLambda (SimpleLambda v1 ty1 b1) (SimpleLambda v2 ty2 b2) = do
                  let local' :: Sem r x -> Sem r x
                      local' = local (HashMap.insert v1 v2)
                  bicheck (go ty1 ty2) (local' (go b1 b2))

                goFunction :: Function -> Function -> Sem r (Maybe MatchError)
                goFunction
                  (Function (FunctionParameter m1 i1 l1) r1)
                  (Function (FunctionParameter m2 i2 l2) r2)
                    | i1 == i2 = do
                        let local' :: Sem r x -> Sem r x
                            local' = case (m1, m2) of
                              (Just v1, Just v2) -> local (HashMap.insert v1 v2)
                              _ -> id
                        bicheck (go l1 l2) (local' (go r1 r2))
                    | otherwise = err

                goLambda :: Lambda -> Lambda -> Sem r (Maybe MatchError)
                goLambda (Lambda l1) (Lambda l2) = case zipExactMay (toList l1) (toList l2) of
                  Just z -> asum <$> mapM (uncurry goClause) z
                  _ -> err
                  where
                    goClause :: LambdaClause -> LambdaClause -> Sem r (Maybe MatchError)
                    goClause (LambdaClause p1 b1) (LambdaClause p2 b2) =
                      case zipExactMay (toList p1) (toList p2) of
                        Nothing -> err
                        Just z -> do
                          m <- ask @(HashMap VarName VarName)
                          (m', patMatch) <- runState m (mapM (uncurry matchPatterns) z)
                          if
                              | and patMatch -> local (const m') (go b1 b2)
                              | otherwise -> err

matchPatterns ::
  forall r.
  (Members '[State InferenceState, State (HashMap VarName VarName), Reader FunctionsTable] r) =>
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
    goConstructor :: ConstructorApp -> ConstructorApp -> Sem r Bool
    goConstructor (ConstructorApp c1 args1) (ConstructorApp c2 args2)
      | c1 /= c2 = err
      | otherwise = case zipExactMay args1 args2 of
          Nothing -> err
          Just z -> allM (uncurry matchPatterns) z

    ok :: Sem r Bool
    ok = return True
    err :: Sem r Bool
    err = return False

runInferenceDefs ::
  (Members '[Error TypeCheckerError, Reader FunctionsTable, State TypesTable] r, HasExpressions funDef) =>
  Sem (Inference ': r) (NonEmpty funDef) ->
  Sem r (NonEmpty funDef)
runInferenceDefs a = do
  ((subs, idens), expr) <- runState iniState (re a) >>= firstM closeState
  let idens' = fillHoles subs <$> idens
  addIdens idens'
  return (subsHoles subs <$> expr)

runInferenceDef ::
  (Members '[Error TypeCheckerError, Reader FunctionsTable, State TypesTable] r, HasExpressions funDef) =>
  Sem (Inference ': r) funDef ->
  Sem r funDef
runInferenceDef = fmap head . runInferenceDefs . fmap pure

addIdens :: (Member (State TypesTable) r) => TypesTable -> Sem r ()
addIdens idens = modify (HashMap.union idens)

-- | Assumes the given function has been type checked
-- | NOTE: Registers the function *only* if the result type is Type
functionDefEval :: forall r'. (Member (Reader FunctionsTable) r') => FunctionDef -> Sem r' (Maybe Expression)
functionDefEval = runFail . goTop
  where
    goTop :: forall r. (Members '[Fail, Reader FunctionsTable] r) => FunctionDef -> Sem r Expression
    goTop f =
      case f ^. funDefClauses of
        c :| [] -> goClause c
        _ -> fail
      where
        goClause :: FunctionClause -> Sem r Expression
        goClause c = do
          let pats = c ^. clausePatterns
              n = length (c ^. clausePatterns)
          (patsTys, _) <- splitNExplicitParams n (f ^. funDefType)
          go (zipExact pats patsTys)
          where
            splitNExplicitParams :: Int -> Expression -> Sem r ([Expression], Expression)
            splitNExplicitParams n fun = do
              let (params, r) = unfoldFunType fun
              unlessM (isUniverse r) fail
              (nfirst, rest) <- failMaybe (splitAtExactMay n params)
              sparams <- mapM simpleExplicitParam nfirst
              let r' = foldFunType rest r
              return (sparams, r')
            isUniverse :: Expression -> Sem r Bool
            isUniverse e = do
              e' <- evalState iniState (weakNormalize' e)
              case e' of
                ExpressionUniverse {} -> return True
                _ -> return False
            simpleExplicitParam :: FunctionParameter -> Sem r Expression
            simpleExplicitParam = \case
              FunctionParameter Nothing Explicit ty -> return ty
              _ -> fail
            goPattern :: (Pattern, Expression) -> Expression -> Sem r Expression
            goPattern (p, ty) = case p of
              PatternVariable v -> return . ExpressionSimpleLambda . SimpleLambda v ty
              _ -> const fail
            go :: [(PatternArg, Expression)] -> Sem r Expression
            go = \case
              [] -> return (c ^. clauseBody)
              ((p, ty) : ps)
                | Implicit <- p ^. patternArgIsImplicit -> fail
                | otherwise -> go ps >>= goPattern (p ^. patternArgPattern, ty)

registerFunctionDef :: (Member (State FunctionsTable) r) => FunctionDef -> Sem r ()
registerFunctionDef f = whenJustM (readerState @FunctionsTable (functionDefEval f)) $ \e ->
  modify (over functionsTable (HashMap.insert (f ^. funDefName) e))
