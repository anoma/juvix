module Juvix.Compiler.Internal.Extra.InstanceInfo
  ( module Juvix.Compiler.Store.Internal.Data.InstanceInfo,
    module Juvix.Compiler.Internal.Extra.InstanceInfo,
    module Juvix.Compiler.Internal.Data.TypedIden,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.TypedIden
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
-- import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Store.Internal.Data.InstanceInfo
import Juvix.Prelude

updateInstanceTable :: InstanceTable -> InstanceInfo -> InstanceTable
updateInstanceTable tab ii@InstanceInfo {..} =
  over instanceTableMap (HashMap.alter go _instanceInfoInductive) tab
  where
    go :: Maybe [InstanceInfo] -> Maybe [InstanceInfo]
    go = \case
      Just is -> Just (ii : is)
      Nothing -> Just [ii]

lookupInstanceTable :: InstanceTable -> Name -> Maybe [InstanceInfo]
lookupInstanceTable tab name = HashMap.lookup name (tab ^. instanceTableMap)

makeRigidParam :: InstanceParam -> InstanceParam
makeRigidParam p = case p of
  InstanceParamVar {} ->
    p
  InstanceParamApp app@InstanceApp {..} ->
    InstanceParamApp $
      app
        { _instanceAppArgs = map makeRigidParam _instanceAppArgs
        }
  InstanceParamFun fn@InstanceFun {..} ->
    InstanceParamFun $
      fn
        { _instanceFunLeft = makeRigidParam _instanceFunLeft,
          _instanceFunRight = makeRigidParam _instanceFunRight
        }
  InstanceParamHole {} ->
    p
  InstanceParamMeta v ->
    InstanceParamVar v

paramToExpression :: (Member NameIdGen r) => InstanceParam -> Sem r Expression
paramToExpression = \case
  InstanceParamVar v ->
    return $ ExpressionIden (IdenVar v)
  InstanceParamApp InstanceApp {..} ->
    return _instanceAppExpression
  InstanceParamFun InstanceFun {..} ->
    return _instanceFunExpression
  InstanceParamHole h ->
    return $ ExpressionHole h
  InstanceParamMeta v ->
    ExpressionHole . mkHole (getLoc v) <$> freshNameId

paramFromExpression :: HashSet VarName -> Expression -> Maybe InstanceParam
paramFromExpression metaVars e = case e of
  ExpressionIden (IdenVar v)
    | HashSet.member v metaVars -> Just $ InstanceParamMeta v
    | otherwise -> Just $ InstanceParamVar v
  ExpressionIden i -> do
    h <- mkInstanceAppHeadIden i
    Just $
      InstanceParamApp $
        InstanceApp
          { _instanceAppHead = h,
            _instanceAppArgs = [],
            _instanceAppExpression = e
          }
  ExpressionHole h -> Just $ InstanceParamHole h
  ExpressionApplication app -> do
    let (h, args) = unfoldApplication app
    args' <- mapM (paramFromExpression metaVars) args
    appHead <- mkInstanceAppHead h
    return $
      InstanceParamApp $
        InstanceApp
          { _instanceAppHead = appHead,
            _instanceAppArgs = toList args',
            _instanceAppExpression = e
          }
  ExpressionFunction Function {..}
    | _functionLeft ^. paramImplicit == Explicit -> do
        l <- paramFromExpression metaVars (_functionLeft ^. paramType)
        r <- paramFromExpression metaVars _functionRight
        return $
          InstanceParamFun
            InstanceFun
              { _instanceFunLeft = l,
                _instanceFunRight = r,
                _instanceFunExpression = e
              }
  _ ->
    Nothing
  where
    mkInstanceAppHeadIden :: Iden -> Maybe InstanceAppHead
    mkInstanceAppHeadIden = \case
      IdenInductive n -> Just (InstanceAppHeadInductive n)
      IdenAxiom n -> Just (InstanceAppHeadAxiom n)
      _ -> Nothing

    mkInstanceAppHead :: Expression -> Maybe InstanceAppHead
    mkInstanceAppHead = \case
      ExpressionIden i -> mkInstanceAppHeadIden i
      _ -> Nothing

traitFromExpression :: HashSet VarName -> Expression -> Maybe InstanceApp
traitFromExpression metaVars e = case paramFromExpression metaVars e of
  Just (InstanceParamApp app) -> Just app
  Just _ -> do
    -- traceM "Just _"
    Nothing
  Nothing -> do
    -- traceM ("Nothing for " <> ppTrace e)
    Nothing

instanceFromTypedIden :: TypedIden -> Maybe InstanceInfo
instanceFromTypedIden TypedIden {..} = do
  InstanceApp {..} <- traitFromExpression metaVars e
  return $
    InstanceInfo
      { _instanceInfoInductive = _instanceAppHead ^. instanceAppHeadName,
        _instanceInfoParams = _instanceAppArgs,
        _instanceInfoResult = _typedIden,
        _instanceInfoArgs = args
      }
  where
    (args, e) = unfoldFunType _typedIdenType
    metaVars = HashSet.fromList $ mapMaybe (^. paramName) args

checkNoMeta :: InstanceParam -> Bool
checkNoMeta = \case
  InstanceParamVar {} -> True
  InstanceParamMeta {} -> False
  InstanceParamHole {} -> True
  InstanceParamApp InstanceApp {..} -> all checkNoMeta _instanceAppArgs
  InstanceParamFun InstanceFun {..} ->
    checkNoMeta _instanceFunLeft && checkNoMeta _instanceFunRight
