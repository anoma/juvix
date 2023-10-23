module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Resolver
  ( isTrait,
    resolveTraitInstance,
    subsumingInstances,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.CoercionInfo
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Data.TypedHole
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude

type SubsI = HashMap VarName InstanceParam

subsIToE :: SubsI -> SubsE
subsIToE = fmap paramToExpression

type CoercionChain = [(CoercionInfo, SubsI)]

isTrait :: InfoTable -> Name -> Bool
isTrait tab name = maybe False (^. inductiveInfoDef . inductiveTrait) (HashMap.lookup name (tab ^. infoInductives))

resolveTraitInstance ::
  (Members '[Error TypeCheckerError, NameIdGen, Inference, Reader InfoTable] r) =>
  TypedHole ->
  Sem r Expression
resolveTraitInstance TypedHole {..} = do
  tbl <- ask
  let tab = foldr (flip updateInstanceTable) (tbl ^. infoInstances) (varsToInstances tbl _typedHoleLocalVars)
  ty <- strongNormalize _typedHoleType
  is <- lookupInstance (tbl ^. infoCoercions) tab ty
  case is of
    [(cs, ii, subs)] ->
      expandArity loc (subsIToE subs) (ii ^. instanceInfoArgs) (ii ^. instanceInfoResult)
        >>= applyCoercions loc cs
    [] ->
      throw (ErrNoInstance (NoInstance ty loc))
    _ ->
      throw (ErrAmbiguousInstances (AmbiguousInstances ty (map snd3 is) loc))
  where
    loc = getLoc _typedHoleHole

subsumingInstances ::
  forall r.
  (Members '[Error TypeCheckerError, Inference] r) =>
  InstanceTable ->
  InstanceInfo ->
  Sem r [(InstanceInfo)]
subsumingInstances tab InstanceInfo {..} = do
  is <- lookupInstance' [] False mempty tab _instanceInfoInductive _instanceInfoParams
  return $
    map snd3 $
      filter (\(_, x, _) -> x ^. instanceInfoResult /= _instanceInfoResult) is

-------------------------------------------------------------------------------------
-- Local functions
-------------------------------------------------------------------------------------

substitutionI :: SubsI -> InstanceParam -> InstanceParam
substitutionI subs p = case p of
  InstanceParamVar {} -> p
  InstanceParamApp InstanceApp {..} ->
    InstanceParamApp
      InstanceApp
        { _instanceAppHead,
          _instanceAppArgs = map (substitutionI subs) _instanceAppArgs,
          _instanceAppExpression = substitutionE (subsIToE subs) _instanceAppExpression
        }
  InstanceParamFun InstanceFun {..} ->
    InstanceParamFun
      InstanceFun
        { _instanceFunLeft = substitutionI subs _instanceFunLeft,
          _instanceFunRight = substitutionI subs _instanceFunRight,
          _instanceFunExpression = substitutionE (subsIToE subs) _instanceFunExpression
        }
  InstanceParamHole {} -> p
  InstanceParamMeta v
    | Just p' <- HashMap.lookup v subs ->
        p'
    | otherwise ->
        p

instanceFromTypedExpression' :: InfoTable -> TypedExpression -> Maybe InstanceInfo
instanceFromTypedExpression' tbl e = do
  ii@InstanceInfo {..} <- instanceFromTypedExpression e
  guard (isTrait tbl _instanceInfoInductive)
  return ii

varsToInstances :: InfoTable -> LocalVars -> [InstanceInfo]
varsToInstances tbl LocalVars {..} =
  mapMaybe
    (instanceFromTypedExpression' tbl . mkTyped)
    (HashMap.toList _localTypes)
  where
    mkTyped :: (VarName, Expression) -> TypedExpression
    mkTyped (v, ty) =
      TypedExpression
        { _typedType = ty,
          _typedExpression = ExpressionIden (IdenVar v)
        }

applyCoercions ::
  (Members '[Error TypeCheckerError, NameIdGen] r) =>
  Interval ->
  CoercionChain ->
  Expression ->
  Sem r Expression
applyCoercions loc cs e =
  foldM (flip (applyCoercion loc)) e (reverse cs)

applyCoercion ::
  (Members '[Error TypeCheckerError, NameIdGen] r) =>
  Interval ->
  (CoercionInfo, SubsI) ->
  Expression ->
  Sem r Expression
applyCoercion loc (CoercionInfo {..}, subs) e = do
  e' <- expandArity loc (subsIToE subs) _coercionInfoArgs _coercionInfoResult
  return $
    ExpressionApplication (Application e' e ImplicitInstance)

expandArity ::
  (Members '[Error TypeCheckerError, NameIdGen] r) =>
  Interval ->
  SubsE ->
  [FunctionParameter] ->
  Expression ->
  Sem r Expression
expandArity loc subs params e = case params of
  [] ->
    return e
  fp@FunctionParameter {..} : params' -> do
    (appr, appi) <-
      if
          | Just (Just t) <- flip HashMap.lookup subs <$> _paramName ->
              return (t, _paramImplicit)
          | _paramImplicit == Implicit -> do
              h <- newHole
              return (ExpressionHole h, Implicit)
          | _paramImplicit == ImplicitInstance -> do
              h <- newHole
              return (ExpressionInstanceHole h, ImplicitInstance)
          | otherwise ->
              throw (ErrExplicitInstanceArgument (ExplicitInstanceArgument fp))
    expandArity loc subs params' (ExpressionApplication (Application e appr appi))
  where
    newHole :: (Member NameIdGen r) => Sem r Hole
    newHole = mkHole loc <$> freshNameId

lookupInstance' ::
  forall r.
  (Member Inference r) =>
  [Name] ->
  Bool ->
  CoercionTable ->
  InstanceTable ->
  Name ->
  [InstanceParam] ->
  Sem r [(CoercionChain, InstanceInfo, SubsI)]
lookupInstance' visited canFillHoles ctab tab name params
  | name `elem` visited = return []
  | otherwise = do
      let is = fromMaybe [] $ lookupInstanceTable tab name
      rs <- mapMaybeM matchInstance is
      case rs of
        [] -> do
          let coes = fromMaybe [] $ lookupCoercionTable ctab name
          concat <$> mapMaybeM matchCoercion coes
        _ -> return rs
  where
    matchInstance :: InstanceInfo -> Sem r (Maybe (CoercionChain, InstanceInfo, SubsI))
    matchInstance ii@InstanceInfo {..} = runFail $ do
      failUnless (length params == length _instanceInfoParams)
      (si, b) <-
        runState mempty $
          and <$> sequence (zipWithExact goMatch _instanceInfoParams params)
      failUnless b
      return ([], ii, si)

    matchCoercion :: CoercionInfo -> Sem r (Maybe [(CoercionChain, InstanceInfo, SubsI)])
    matchCoercion ci@CoercionInfo {..} = runFail $ do
      failUnless (length params == length _coercionInfoParams)
      (si, b) <-
        runState mempty $
          and <$> sequence (zipWithExact goMatch _coercionInfoParams params)
      failUnless b
      let name' = _coercionInfoTarget ^. instanceAppHead
          args' = map (substitutionI si) (_coercionInfoTarget ^. instanceAppArgs)
      is <- lookupInstance' (name : visited) canFillHoles ctab tab name' args'
      return $ map (first3 ((ci, si) :)) is

    goMatch :: InstanceParam -> InstanceParam -> Sem (State SubsI ': Fail ': r) Bool
    goMatch pat t = case (pat, t) of
      (InstanceParamMeta v, _) -> do
        m <- gets (HashMap.lookup v)
        case m of
          Just t'
            | t' == t ->
                return True
            | otherwise ->
                return False
          Nothing -> do
            modify (HashMap.insert v t)
            return True
      (InstanceParamVar v1, InstanceParamVar v2)
        | v1 == v2 ->
            return True
      (InstanceParamHole h, _)
        | canFillHoles -> do
            m <- matchTypes (ExpressionHole h) (paramToExpression t)
            case m of
              Just {} -> return False
              Nothing -> return True
        | otherwise ->
            return False
      (_, InstanceParamHole h)
        | canFillHoles && checkNoMeta pat -> do
            m <- matchTypes (paramToExpression pat) (ExpressionHole h)
            case m of
              Just {} -> return False
              Nothing -> return True
      (InstanceParamApp app1, InstanceParamApp app2)
        | app1 ^. instanceAppHead == app2 ^. instanceAppHead -> do
            and <$> sequence (zipWithExact goMatch (app1 ^. instanceAppArgs) (app2 ^. instanceAppArgs))
      (InstanceParamFun fun1, InstanceParamFun fun2) -> do
        l <- goMatch (fun1 ^. instanceFunLeft) (fun2 ^. instanceFunLeft)
        r <- goMatch (fun1 ^. instanceFunRight) (fun2 ^. instanceFunRight)
        return $ l && r
      (InstanceParamVar {}, _) -> return False
      (InstanceParamApp {}, _) -> return False
      (InstanceParamFun {}, _) -> return False

lookupInstance ::
  forall r.
  (Members '[Error TypeCheckerError, Inference] r) =>
  CoercionTable ->
  InstanceTable ->
  Expression ->
  Sem r [(CoercionChain, InstanceInfo, SubsI)]
lookupInstance ctab tab ty = do
  case traitFromExpression mempty ty of
    Just InstanceApp {..} ->
      lookupInstance' [] False ctab tab _instanceAppHead _instanceAppArgs
    _ ->
      return []
