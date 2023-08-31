module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Extra where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error
import Juvix.Prelude

updateInstanceTable :: InstanceTable -> InstanceInfo -> InstanceTable
updateInstanceTable tab ii@InstanceInfo {..} =
  HashMap.alter go _instanceInfoInductive tab
  where
    go :: Maybe [InstanceInfo] -> Maybe [InstanceInfo]
    go = \case
      Just is -> Just (ii : is)
      Nothing -> Just [ii]

type SubsI = HashMap VarName InstanceParam

lookupInstance' ::
  InstanceTable ->
  Name ->
  [InstanceParam] ->
  [(InstanceInfo, SubsI)]
lookupInstance' tab name params = do
  let is = fromMaybe [] $ HashMap.lookup name tab
  mapMaybe matchInstance is
  where
    matchInstance :: InstanceInfo -> Maybe (InstanceInfo, SubsI)
    matchInstance ii@InstanceInfo {..}
      | length params /= length _instanceInfoParams =
          Nothing
      | otherwise =
          let (mp, b) =
                run $
                  runState mempty $
                    and <$> sequence (zipWithExact goMatch _instanceInfoParams params)
           in if
                  | b -> Just (ii, mp)
                  | otherwise -> Nothing

    goMatch :: (Member (State SubsI) r) => InstanceParam -> InstanceParam -> Sem r Bool
    goMatch pat t = case (pat, t) of
      (InstanceParamMeta (InstanceMeta v), _) -> do
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
      (InstanceParamVar (InstanceVar v1), InstanceParamVar (InstanceVar v2))
        | v1 == v2 ->
            return True
      (InstanceParamApp (InstanceApp h1 args1), InstanceParamApp (InstanceApp h2 args2))
        | h1 == h2 -> do
            and <$> sequence (zipWithExact goMatch args1 args2)
      _ ->
        return False

lookupInstance ::
  forall r.
  (Members '[Error TraitError, Reader InfoTable] r) =>
  InstanceTable ->
  Expression ->
  Sem r [(InstanceInfo, SubsI)]
lookupInstance tab ty = do
  tbl <- ask
  case traitFromExpression True ty of
    Just (InstanceApp h args)
      | fromJust (HashMap.lookup h (tbl ^. infoInductives)) ^. inductiveInfoDef . inductiveTrait ->
          return $ lookupInstance' tab h args
    _ ->
      throw (ErrNotATrait (NotATrait ty))

paramFromExpression :: Bool -> Expression -> Maybe InstanceParam
paramFromExpression bRigid = \case
  ExpressionIden (IdenInductive n) ->
    Just $ InstanceParamApp $ InstanceApp n []
  ExpressionIden (IdenVar v)
    | bRigid -> Just $ InstanceParamVar $ InstanceVar v
    | otherwise -> Just $ InstanceParamMeta $ InstanceMeta v
  ExpressionApplication app -> do
    let (h, args) = unfoldApplication app
    args' <- mapM (paramFromExpression bRigid) args
    case h of
      ExpressionIden (IdenInductive n) ->
        return $ InstanceParamApp $ InstanceApp n (toList args')
      _ ->
        Nothing
  _ ->
    Nothing

paramToExpression :: InstanceParam -> Expression
paramToExpression = \case
  InstanceParamVar (InstanceVar v) ->
    ExpressionIden (IdenVar v)
  InstanceParamApp (InstanceApp h args) ->
    foldExplicitApplication (ExpressionIden (IdenInductive h)) (map paramToExpression args)
  InstanceParamMeta {} ->
    impossible

traitFromExpression :: Bool -> Expression -> Maybe InstanceApp
traitFromExpression bRigid e = case paramFromExpression bRigid e of
  Just (InstanceParamApp app) -> Just app
  _ -> Nothing

instanceFromTypedExpression :: Bool -> TypedExpression -> Maybe InstanceInfo
instanceFromTypedExpression bRigid TypedExpression {..} = case traitFromExpression bRigid e of
  Just (InstanceApp h params) ->
    Just $
      InstanceInfo
        { _instanceInfoInductive = h,
          _instanceInfoParams = params,
          _instanceInfoResult = _typedExpression,
          _instanceInfoArgs = args
        }
  Nothing -> Nothing
  where
    (args, e) = unfoldFunType _typedType

substParam :: SubsI -> InstanceParam -> InstanceParam
substParam subs p = case p of
  InstanceParamVar {} -> p
  InstanceParamApp app ->
    InstanceParamApp (over instanceAppArgs (map (substParam subs)) app)
  InstanceParamMeta (InstanceMeta v) ->
    fromMaybe p (HashMap.lookup v subs)

subsIToE :: SubsI -> SubsE
subsIToE = fmap paramToExpression
