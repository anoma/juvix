module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Resolver
  ( isTrait,
    resolveTraitInstance,
    subsumingInstances,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Data.TypedInstanceHole
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Extra.CoercionInfo
import Juvix.Compiler.Internal.Extra.InstanceInfo
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.ResultBuilder
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Store.Scoped.Data.InfoTable (BuiltinsTable)
import Juvix.Prelude

type SubsI = HashMap VarName InstanceParam

subsIToE :: (Member NameIdGen r) => SubsI -> Sem r Subs
subsIToE = mapM paramToExpression

type CoercionChain = [(CoercionInfo, SubsI)]

isTrait :: InfoTable -> Name -> Bool
isTrait tab name = maybe False (^. inductiveInfoTrait) (HashMap.lookup name (tab ^. infoInductives))

resolveTraitInstance ::
  (Members '[Error TypeCheckerError, NameIdGen, Inference, ResultBuilder, Reader BuiltinsTable, Reader InfoTable] r) =>
  TypedInstanceHole ->
  Sem r Expression
resolveTraitInstance TypedInstanceHole {..} = do
  vars :: LocalVars <- overM localTypes (mapM strongNormalize_) _typedInstanceHoleLocalVars
  infoTab <- ask
  combtabs <- getCombinedTables
  vars2instances :: [InstanceInfo] <- varsToInstances infoTab vars
  let tab0 = combtabs ^. typeCheckingTablesInstanceTable
      tab = foldr (flip updateInstanceTable) tab0 vars2instances
      ctab = combtabs ^. typeCheckingTablesCoercionTable
  ty <- strongNormalize _typedInstanceHoleType
  traceM ("lookup instance : " <> ppTrace (ty ^. normalizedExpression))
  is <- lookupInstance ctab tab (ty ^. normalizedExpression)
  case is of
    [(cs, ii, subs)] -> do
      subs' <- subsIToE subs
      expandArity' loc subs' (ii ^. instanceInfoArgs) (ii ^. instanceInfoResult)
        >>= applyCoercions loc cs
    [] ->
      throw (ErrNoInstance (NoInstance ty loc))
    _ ->
      throw (ErrAmbiguousInstances (AmbiguousInstances ty (map snd3 is) loc))
  where
    loc = getLoc _typedInstanceHoleHole

subsumingInstances ::
  forall r.
  (Members '[Error TypeCheckerError, Inference, NameIdGen] r) =>
  InstanceTable ->
  InstanceInfo ->
  Sem r [(InstanceInfo)]
subsumingInstances tab InstanceInfo {..} = do
  is <- lookupInstance' [] mempty tab _instanceInfoInductive (map makeRigidParam _instanceInfoParams)
  return $
    map snd3 $
      filter (\(_, x, _) -> x ^. instanceInfoResult /= _instanceInfoResult) is

-------------------------------------------------------------------------------------
-- Local functions
-------------------------------------------------------------------------------------

substitutionI :: (Member NameIdGen r) => SubsI -> InstanceParam -> Sem r InstanceParam
substitutionI subs p = case p of
  InstanceParamVar {} -> return p
  InstanceParamNatural n -> InstanceParamNatural <$> instanceNatArg (substitutionI subs) n
  InstanceParamApp InstanceApp {..} -> do
    args <- mapM (substitutionI subs) _instanceAppArgs
    subs' <- subsIToE subs
    e <- substitutionE subs' _instanceAppExpression
    return $
      InstanceParamApp
        InstanceApp
          { _instanceAppHead,
            _instanceAppArgs = args,
            _instanceAppExpression = e
          }
  InstanceParamFun InstanceFun {..} -> do
    l <- substitutionI subs _instanceFunLeft
    r <- substitutionI subs _instanceFunRight
    subs' <- subsIToE subs
    e <- substitutionE subs' _instanceFunExpression
    return $
      InstanceParamFun
        InstanceFun
          { _instanceFunLeft = l,
            _instanceFunRight = r,
            _instanceFunExpression = e
          }
  InstanceParamHole h
    | Just p' <- HashMap.lookup (varFromHole h) subs ->
        -- we don't need to clone here because `InstanceParamHole` doesn't have binders
        return p'
    | otherwise ->
        return p
  InstanceParamMeta v
    | Just p' <- HashMap.lookup v subs ->
        -- we don't need to clone here because `InstanceParamMeta` doesn't have binders
        return p'
    | otherwise ->
        return p

instanceFromTypedIden' :: (Members '[Reader BuiltinsTable] r) => InfoTable -> TypedIden -> Sem (Fail ': r) InstanceInfo
instanceFromTypedIden' tbl e = do
  ii@InstanceInfo {..} <- instanceFromTypedIden e
  failUnless (isTrait tbl _instanceInfoInductive)
  return ii

varsToInstances :: (Members '[Reader BuiltinsTable] r) => InfoTable -> LocalVars -> Sem r [InstanceInfo]
varsToInstances tbl LocalVars {..} =
  mapMaybeM
    (runFail . instanceFromTypedIden' tbl . mkTyped)
    (HashMap.toList _localTypes)
  where
    mkTyped :: (VarName, Expression) -> TypedIden
    mkTyped (v, ty) =
      TypedIden
        { _typedIdenType = ty,
          _typedIden = IdenVar v
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
  subs' <- subsIToE subs
  e' <- expandArity' loc subs' _coercionInfoArgs _coercionInfoResult
  return $
    ExpressionApplication (Application e' e ImplicitInstance)

expandArity' ::
  (Members '[Error TypeCheckerError, NameIdGen] r) =>
  Interval ->
  Subs ->
  [FunctionParameter] ->
  Iden ->
  Sem r Expression
expandArity' loc subs params iden =
  expandArity loc subs params (ExpressionIden (set (idenName . nameLoc) loc iden))

expandArity ::
  (Members '[Error TypeCheckerError, NameIdGen] r) =>
  Interval ->
  Subs ->
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
              h <- newInstanceHole
              return (ExpressionInstanceHole h, ImplicitInstance)
          | otherwise ->
              throw (ErrExplicitInstanceArgument (ExplicitInstanceArgument fp))
    expandArity loc subs params' (ExpressionApplication (Application e appr appi))
  where
    newInstanceHole :: (Member NameIdGen r) => Sem r InstanceHole
    newInstanceHole = mkInstanceHole loc <$> freshNameId
    newHole :: (Member NameIdGen r) => Sem r Hole
    newHole = mkHole loc <$> freshNameId

lookupInstance' ::
  forall r.
  (Members '[NameIdGen] r) =>
  [Name] ->
  CoercionTable ->
  InstanceTable ->
  Name ->
  [InstanceParam] ->
  Sem r [(CoercionChain, InstanceInfo, SubsI)]
lookupInstance' visited ctab tab name params
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
          andM (zipWithExact (goMatch True) _instanceInfoParams params)
      failUnless b
      return ([], ii, si)

    matchCoercion :: CoercionInfo -> Sem r (Maybe [(CoercionChain, InstanceInfo, SubsI)])
    matchCoercion ci@CoercionInfo {..} = runFail $ do
      failUnless (length params == length _coercionInfoParams)
      (si, b) <-
        runState mempty $
          and <$> sequence (zipWithExact (goMatch True) _coercionInfoParams params)
      failUnless b
      let name' = _coercionInfoTarget ^. instanceAppHead . instanceAppHeadName
      args' <- mapM (substitutionI si) (_coercionInfoTarget ^. instanceAppArgs)
      let visited'
            | _coercionInfoDecreasing = visited
            | otherwise = name : visited
      is <- lookupInstance' visited' ctab tab name' args'
      return $ map (first3 ((ci, si) :)) is

    goMatch :: Bool -> InstanceParam -> InstanceParam -> Sem (State SubsI ': Fail ': r) Bool
    goMatch assignMetas pat t = case (pat, t) of
      (InstanceParamMeta v, _)
        | assignMetas -> goMatchMeta v t
        | otherwise -> return True
      (_, InstanceParamMeta {}) -> return True
      (_, InstanceParamHole {}) -> return True
      (InstanceParamNatural v1, InstanceParamNatural v2) ->
        andM
          [ return (v1 ^. instanceNatSuc == v2 ^. instanceNatSuc),
            goMatch assignMetas (v1 ^. instanceNatArg) (v2 ^. instanceNatArg)
          ]
      (InstanceParamVar v1, InstanceParamVar v2)
        | v1 == v2 -> return True
      (InstanceParamApp app1, InstanceParamApp app2)
        | app1 ^. instanceAppHead == app2 ^. instanceAppHead -> do
            andM (zipWithExact (goMatch assignMetas) (app1 ^. instanceAppArgs) (app2 ^. instanceAppArgs))
      (InstanceParamFun fun1, InstanceParamFun fun2) -> do
        l <- goMatch assignMetas (fun1 ^. instanceFunLeft) (fun2 ^. instanceFunLeft)
        r <- goMatch assignMetas (fun1 ^. instanceFunRight) (fun2 ^. instanceFunRight)
        return $ l && r
      (InstanceParamNatural {}, _) -> return False
      (InstanceParamVar {}, _) -> return False
      (InstanceParamApp {}, _) -> return False
      (InstanceParamFun {}, _) -> return False
      (InstanceParamHole {}, _) -> return False

    goMatchMeta :: VarName -> InstanceParam -> Sem (State SubsI ': Fail ': r) Bool
    goMatchMeta v t = do
      m <- gets (HashMap.lookup v)
      case m of
        Just t'
          | t' == t ->
              return True
          | otherwise ->
              -- Here, we need to match without assigning meta-variables to
              -- avoid possible infinite loops
              goMatch False t' t
        Nothing -> do
          modify (HashMap.insert v t)
          return True

-- TODO where to use this?
-- squashInstanceNat :: InstanceNat -> Either InstanceNat InstanceParam
-- squashInstanceNat n
--   | n ^. instanceNatSuc == 0 = case n ^. instanceNatArg of
--       InstanceParamNatural n' -> squashInstanceNat n'
--       m -> Right m
--   | otherwise = case n ^. instanceNatArg of
--       InstanceParamNatural n' -> case squashInstanceNat n' of
--         Right s -> Left (set instanceNatArg s n)
--         Left s ->
--           Left
--             InstanceNat
--               { _instanceNatSuc = n ^. instanceNatSuc + s ^. instanceNatSuc,
--                 _instanceNatArg = s ^. instanceNatArg,
--                 _instanceNatLoc = n ^. instanceNatLoc <> s ^. instanceNatLoc
--               }
--       m -> Right m

lookupInstance ::
  forall r.
  (Members '[Error TypeCheckerError, Inference, NameIdGen, Reader BuiltinsTable] r) =>
  CoercionTable ->
  InstanceTable ->
  Expression ->
  Sem r [(CoercionChain, InstanceInfo, SubsI)]
lookupInstance ctab tab ty = do
  m <- runFail (traitFromExpression mempty ty)
  case m of
    Just InstanceApp {..} -> do
      traceM "instanceApp"
      lookupInstance' [] ctab tab (_instanceAppHead ^. instanceAppHeadName) _instanceAppArgs
    _ -> do
      traceM ("empty: " <> ppTrace ty)
      return []
