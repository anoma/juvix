module MiniJuvix.Syntax.MicroJuvix.TypeChecker.Inference where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.MicroJuvix.Error
import MiniJuvix.Syntax.MicroJuvix.Language.Extra

data MetavarState
  = Fresh
  | -- | Type may contain holes
    Refined Type

data Inference m a where
  FreshMetavar :: Hole -> Inference m TypedExpression
  MatchTypes :: Type -> Type -> Inference m Bool

makeSem ''Inference

newtype InferenceState = InferenceState
  { _inferenceMap :: HashMap Hole MetavarState
  }

makeLenses ''InferenceState

iniState :: InferenceState
iniState = InferenceState mempty

closeState :: Member (Error TypeCheckerError) r => InferenceState -> Sem r (HashMap Hole Type)
closeState = \case
  InferenceState m -> execState mempty (f m)
  where
    f ::
      forall r'.
      Members '[Error TypeCheckerError, State (HashMap Hole Type)] r' =>
      HashMap Hole MetavarState ->
      Sem r' ()
    f m = mapM_ (uncurry goHole) (HashMap.toList m)
      where
        goHole :: Hole -> MetavarState -> Sem r' Type
        goHole h = \case
          Fresh -> throw (ErrUnsolvedMeta (UnsolvedMeta h))
          Refined t -> do
            s <- gets @(HashMap Hole Type) (^. at h)
            case s of
              Just noHolesTy -> return noHolesTy
              Nothing -> do
                x <- goType t
                modify (HashMap.insert h x)
                return x
        goType :: Type -> Sem r' Type
        goType t = case t of
          TypeIden {} -> return t
          TypeApp (TypeApplication a b i) -> do
            a' <- goType a
            b' <- goType b
            return (TypeApp (TypeApplication a' b' i))
          TypeFunction (Function a b) -> do
            a' <- goType a
            b' <- goType b
            return (TypeFunction (Function a' b'))
          TypeAbs (TypeAbstraction v i b) -> TypeAbs . TypeAbstraction v i <$> goType b
          TypeUniverse -> return TypeUniverse
          TypeAny -> return TypeAny
          TypeHole h' ->
            let st = fromJust (m ^. at h')
             in goHole h' st

getMetavar :: Member (State InferenceState) r => Hole -> Sem r MetavarState
getMetavar h = gets (fromJust . (^. inferenceMap . at h))

re :: Member (Error TypeCheckerError) r => Sem (Inference ': r) a -> Sem (State InferenceState ': r) a
re = reinterpret $ \case
  FreshMetavar h -> freshMetavar' h
  MatchTypes a b -> matchTypes' a b
  where
    queryMetavar' :: Members '[State InferenceState] r => Hole -> Sem r (Maybe Type)
    queryMetavar' h = metavarType <$> getMetavar h

    freshMetavar' :: Members '[State InferenceState] r => Hole -> Sem r TypedExpression
    freshMetavar' h = do
      modify (over inferenceMap (HashMap.insert h Fresh))
      return
        TypedExpression
          { _typedExpression = ExpressionHole h,
            _typedType = TypeUniverse
          }

    refineFreshMetavar ::
      Members '[Error TypeCheckerError, State InferenceState] r =>
      Hole ->
      Type ->
      Sem r ()
    refineFreshMetavar h t = do
      s <- gets (fromJust . (^. inferenceMap . at h))
      case s of
        Fresh -> modify (over inferenceMap (HashMap.insert h (Refined t)))
        Refined {} -> impossible

    metavarType :: MetavarState -> Maybe Type
    metavarType = \case
      Fresh -> Nothing
      Refined t -> Just t

    -- Supports alpha equivalence.
    matchTypes' :: Members '[Error TypeCheckerError, State InferenceState] r => Type -> Type -> Sem r Bool
    matchTypes' ty = runReader ini . go ty
      where
        ini :: HashMap VarName VarName
        ini = mempty
        go ::
          forall r.
          Members '[Error TypeCheckerError, State InferenceState, Reader (HashMap VarName VarName)] r =>
          Type ->
          Type ->
          Sem r Bool
        go a' b' = case (a', b') of
          (TypeIden a, TypeIden b) -> goIden a b
          (TypeApp a, TypeApp b) -> goApp a b
          (TypeAbs a, TypeAbs b) -> goAbs a b
          (TypeFunction a, TypeFunction b) -> goFunction a b
          (TypeUniverse, TypeUniverse) -> return True
          (TypeAny, _) -> return True
          (_, TypeAny) -> return True
          (TypeHole h, a) -> goHole h a
          (a, TypeHole h) -> goHole h a
          -- TODO is the final wildcard bad style?
          -- what if more Type constructors are added
          _ -> return False
          where
            goHole :: Hole -> Type -> Sem r Bool
            goHole h t = do
              r <- queryMetavar' h
              case r of
                Nothing -> refineFreshMetavar h t $> True
                Just ht -> matchTypes' t ht
            goIden :: TypeIden -> TypeIden -> Sem r Bool
            goIden ia ib = case (ia, ib) of
              (TypeIdenInductive a, TypeIdenInductive b) -> return (a == b)
              (TypeIdenAxiom a, TypeIdenAxiom b) -> return (a == b)
              (TypeIdenVariable a, TypeIdenVariable b) -> do
                mappedEq <- (== Just b) . HashMap.lookup a <$> ask
                return (a == b || mappedEq)
              _ -> return False
            goApp :: TypeApplication -> TypeApplication -> Sem r Bool
            goApp (TypeApplication f x _) (TypeApplication f' x' _) = andM [go f f', go x x']
            goFunction :: Function -> Function -> Sem r Bool
            goFunction (Function l r) (Function l' r') = andM [go l l', go r r']
            goAbs :: TypeAbstraction -> TypeAbstraction -> Sem r Bool
            goAbs (TypeAbstraction v1 _ r) (TypeAbstraction v2 _ r') =
              local (HashMap.insert v1 v2) (go r r')

runInference :: Member (Error TypeCheckerError) r => Sem (Inference ': r) Expression -> Sem r Expression
runInference a = do
  (subs, expr) <- runState iniState (re a) >>= firstM closeState
  return (fillHoles subs expr)

runInferenceDef :: Member (Error TypeCheckerError) r => Sem (Inference ': r) FunctionDef -> Sem r FunctionDef
runInferenceDef a = do
  (subs, expr) <- runState iniState (re a) >>= firstM closeState
  return (fillHolesFunctionDef subs expr)
