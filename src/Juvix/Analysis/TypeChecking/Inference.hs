module Juvix.Analysis.TypeChecking.Inference
  ( module Juvix.Analysis.TypeChecking.Inference,
    module Juvix.Analysis.TypeChecking.FunctionsTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Analysis.TypeChecking.FunctionsTable
import Juvix.Prelude hiding (fromEither)
import Juvix.Syntax.MicroJuvix.Error
import Juvix.Syntax.MicroJuvix.Language.Extra
import Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult

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

makeSem ''Inference

data InferenceState = InferenceState
  { _inferenceMap :: HashMap Hole MetavarState,
    _inferenceIdens :: TypesTable
  }

makeLenses ''InferenceState

iniState :: InferenceState
iniState = InferenceState mempty mempty

closeState ::
  Member (Error TypeCheckerError) r =>
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
      Members '[Error TypeCheckerError, State (HashMap Hole Expression)] r' =>
      HashMap Hole MetavarState ->
      Sem r' ()
    f m = mapM_ goHole (HashMap.keys m)
      where
        goHole :: Hole -> Sem r' Expression
        goHole h =
          let st = fromJust (m ^. at h)
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

getMetavar :: Member (State InferenceState) r => Hole -> Sem r MetavarState
getMetavar h = gets (fromJust . (^. inferenceMap . at h))

strongNormalize' :: forall r. Members '[Reader FunctionsTable, State InferenceState] r => Expression -> Sem r Expression
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
      ExpressionLambda l -> ExpressionLambda <$> goLambda l

    goLambda :: Lambda -> Sem r Lambda
    goLambda (Lambda lamVar lamTy lamBody) = do
      lamTy' <- go lamTy
      lamBody' <- go lamBody
      return (Lambda lamVar lamTy' lamBody')

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
        ExpressionLambda (Lambda lamVar _ lamBody) -> do
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

weakNormalize' :: forall r. Members '[Reader FunctionsTable, State InferenceState] r => Expression -> Sem r Expression
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
      ExpressionLambda {} -> return e
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
        ExpressionLambda (Lambda lamVar _ lamBody) -> do
          go (substitutionE (HashMap.singleton lamVar r) lamBody)
        _ -> return (ExpressionApplication (Application l' r i))
    goHole :: Hole -> Sem r Expression
    goHole h = do
      s <- getMetavar h
      case s of
        Fresh -> return (ExpressionHole h)
        Refined r -> go r

freshMetavar :: Members '[Inference] r => Hole -> Sem r ()
freshMetavar = void . queryMetavar

queryMetavar' :: Members '[State InferenceState] r => Hole -> Sem r (Maybe Expression)
queryMetavar' h = do
  m <- gets (^. inferenceMap . at h)
  case m of
    Nothing -> do
      modify (over inferenceMap (HashMap.insert h Fresh))
      return Nothing
    Just Fresh -> return Nothing
    Just (Refined e) -> return (Just e)

re ::
  Members '[Error TypeCheckerError, Reader FunctionsTable] r =>
  Sem (Inference ': r) a ->
  Sem (State InferenceState ': r) a
re = reinterpret $ \case
  MatchTypes a b -> matchTypes' a b
  QueryMetavar h -> queryMetavar' h
  RegisterIden i ty -> registerIden' i ty
  StrongNormalize ty -> strongNormalize' ty
  where
    registerIden' :: Members '[State InferenceState] r => Name -> Expression -> Sem r ()
    registerIden' i ty = modify (over inferenceIdens (HashMap.insert i ty))

    refineFreshMetavar ::
      Members '[Error TypeCheckerError, State InferenceState] r =>
      Hole ->
      Expression ->
      Sem r ()
    refineFreshMetavar h t
      | ExpressionHole h' <- t, h' == h = return ()
      | otherwise =
          do
            s <- gets (fromJust . (^. inferenceMap . at h))
            case s of
              Fresh -> modify (over inferenceMap (HashMap.insert h (Refined t)))
              Refined {} -> impossible

    -- Supports alpha equivalence.
    matchTypes' :: Members '[Error TypeCheckerError, State InferenceState, Reader FunctionsTable] r => Expression -> Expression -> Sem r (Maybe MatchError)
    matchTypes' ty = runReader ini . go ty
      where
        ini :: HashMap VarName VarName
        ini = mempty
        go ::
          forall r.
          Members '[Error TypeCheckerError, State InferenceState, Reader (HashMap VarName VarName), Reader FunctionsTable] r =>
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
                (ExpressionLambda a, ExpressionLambda b) -> goLambda a b
                (ExpressionHole h, a) -> goHole h a
                (a, ExpressionHole h) -> goHole h a
                (ExpressionLambda {}, _) -> err
                (_, ExpressionLambda {}) -> err
                (ExpressionIden {}, _) -> err
                (_, ExpressionIden {}) -> err
                (ExpressionApplication {}, _) -> err
                (_, ExpressionApplication {}) -> err
                (ExpressionFunction {}, _) -> err
                (_, ExpressionFunction {}) -> err
                (ExpressionUniverse {}, _) -> err
                (_, ExpressionUniverse {}) -> err
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
                goLambda :: Lambda -> Lambda -> Sem r (Maybe MatchError)
                goLambda (Lambda v1 ty1 b1) (Lambda v2 ty2 b2) = do
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
                    | otherwise = ok

runInferenceDef ::
  (Members '[Error TypeCheckerError, Reader FunctionsTable, State TypesTable] r, HasExpressions funDef) =>
  Sem (Inference ': r) funDef ->
  Sem r funDef
runInferenceDef a = do
  ((subs, idens), expr) <- runState iniState (re a) >>= firstM closeState
  let idens' = fillHoles subs <$> idens
  addIdens idens'
  return (subsHoles subs expr)

addIdens :: Member (State TypesTable) r => TypesTable -> Sem r ()
addIdens idens = modify (HashMap.union idens)

-- | Assumes the given function has been type checked
-- | NOTE: Registers the function *only* if the result type is Type
functionDefEval :: forall r'. Member (Reader FunctionsTable) r' => FunctionDef -> Sem r' (Maybe Expression)
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
              PatternVariable v -> return . ExpressionLambda . Lambda v ty
              _ -> const fail
            go :: [(PatternArg, Expression)] -> Sem r Expression
            go = \case
              [] -> return (c ^. clauseBody)
              ((p, ty) : ps)
                | Implicit <- p ^. patternArgIsImplicit -> fail
                | otherwise -> go ps >>= goPattern (p ^. patternArgPattern, ty)

registerFunctionDef :: Member (State FunctionsTable) r => FunctionDef -> Sem r ()
registerFunctionDef f = whenJustM (readerState @FunctionsTable (functionDefEval f)) $ \e ->
  modify (over functionsTable (HashMap.insert (f ^. funDefName) e))
