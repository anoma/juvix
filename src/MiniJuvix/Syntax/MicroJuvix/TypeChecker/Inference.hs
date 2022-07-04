module MiniJuvix.Syntax.MicroJuvix.TypeChecker.Inference where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.MicroJuvix.Error
import MiniJuvix.Syntax.MicroJuvix.Language.Extra

data MetavarState
  = Fresh
  | -- | Type may contain holes
    Refined Expression

data Inference m a where
  FreshMetavar :: Hole -> Inference m TypedExpression
  MatchTypes :: Expression -> Expression -> Inference m Bool
  QueryMetavar :: Hole -> Inference m (Maybe Expression)

makeSem ''Inference

newtype InferenceState = InferenceState
  { _inferenceMap :: HashMap Hole MetavarState
  }

makeLenses ''InferenceState

iniState :: InferenceState
iniState = InferenceState mempty

closeState :: Member (Error TypeCheckerError) r => InferenceState -> Sem r (HashMap Hole Expression)
closeState = \case
  InferenceState m -> execState mempty (f m)
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

re :: Member (Error TypeCheckerError) r => Sem (Inference ': r) a -> Sem (State InferenceState ': r) a
re = reinterpret $ \case
  FreshMetavar h -> freshMetavar' h
  MatchTypes a b -> matchTypes' a b
  QueryMetavar h -> queryMetavar' h
  where
    queryMetavar' :: Members '[State InferenceState] r => Hole -> Sem r (Maybe Expression)
    queryMetavar' h = do
      s <- getMetavar h
      case s of
        Fresh -> return Nothing
        Refined t -> return (Just t)

    freshMetavar' :: Members '[State InferenceState] r => Hole -> Sem r TypedExpression
    freshMetavar' h = do
      modify (over inferenceMap (HashMap.insert h Fresh))
      return
        TypedExpression
          { _typedExpression = ExpressionHole h,
            _typedType = ExpressionUniverse (SmallUniverse (getLoc h))
          }

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
    matchTypes' :: Members '[Error TypeCheckerError, State InferenceState] r => Expression -> Expression -> Sem r Bool
    matchTypes' ty = runReader ini . go ty
      where
        ini :: HashMap VarName VarName
        ini = mempty
        go ::
          forall r.
          Members '[Error TypeCheckerError, State InferenceState, Reader (HashMap VarName VarName)] r =>
          Expression ->
          Expression ->
          Sem r Bool
        go a' b' = case (a', b') of
          (ExpressionIden a, ExpressionIden b) -> goIden a b
          (ExpressionApplication a, ExpressionApplication b) -> goApplication a b
          (ExpressionFunction a, ExpressionFunction b) -> goFunction a b
          (ExpressionUniverse u, ExpressionUniverse u') -> return (u == u')
          (ExpressionHole h, a) -> goHole h a
          (a, ExpressionHole h) -> goHole h a
          (ExpressionIden {}, _) -> return False
          (_, ExpressionIden {}) -> return False
          (ExpressionApplication {}, _) -> return False
          (_, ExpressionApplication {}) -> return False
          (ExpressionFunction {}, _) -> return False
          (_, ExpressionFunction {}) -> return False
          (ExpressionUniverse {}, _) -> return False
          (_, ExpressionUniverse {}) -> return False
          (ExpressionLiteral l, ExpressionLiteral l') -> return (l == l')
          where
            goHole :: Hole -> Expression -> Sem r Bool
            goHole h t = do
              r <- queryMetavar' h
              case r of
                Nothing -> refineFreshMetavar h t $> True
                Just ht -> matchTypes' t ht
            goIden :: Iden -> Iden -> Sem r Bool
            goIden ia ib = case (ia, ib) of
              (IdenInductive a, IdenInductive b) -> return (a == b)
              (IdenAxiom a, IdenAxiom b) -> return (a == b)
              (IdenVar a, IdenVar b) -> do
                mappedEq <- (== Just b) . HashMap.lookup a <$> ask
                return (a == b || mappedEq)
              _ -> return False
            goApplication :: Application -> Application -> Sem r Bool
            goApplication (Application f x _) (Application f' x' _) = andM [go f f', go x x']
            goFunction :: Function -> Function -> Sem r Bool
            goFunction
              (Function (FunctionParameter m1 i1 l1) r1)
              (Function (FunctionParameter m2 i2 l2) r2)
                | i1 == i2 = do
                    let local' :: Sem r x -> Sem r x
                        local' = case (m1, m2) of
                          (Just v1, Just v2) -> local (HashMap.insert v1 v2)
                          _ -> id
                    andM [go l1 l2, local' (go r1 r2)]
                | otherwise = return False

runInference :: Member (Error TypeCheckerError) r => Sem (Inference ': r) Expression -> Sem r Expression
runInference a = do
  (subs, expr) <- runState iniState (re a) >>= firstM closeState
  return (fillHoles subs expr)

runInferenceDef :: Member (Error TypeCheckerError) r => Sem (Inference ': r) FunctionDef -> Sem r FunctionDef
runInferenceDef a = do
  (subs, expr) <- runState iniState (re a) >>= firstM closeState
  return (fillHolesFunctionDef subs expr)
