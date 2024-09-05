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

paramToExpression :: InstanceParam -> Expression
paramToExpression = \case
  InstanceParamVar v ->
    ExpressionIden (IdenVar v)
  InstanceParamApp InstanceApp {..} ->
    _instanceAppExpression
  InstanceParamFun InstanceFun {..} ->
    _instanceFunExpression
  InstanceParamHole h ->
    ExpressionHole h
  InstanceParamMeta v ->
    ExpressionIden (IdenVar v)

paramFromExpression :: HashSet VarName -> Expression -> Maybe InstanceParam
paramFromExpression metaVars e = case e of
  ExpressionIden (IdenInductive n) ->
    Just $
      InstanceParamApp $
        InstanceApp
          { _instanceAppHead = n,
            _instanceAppArgs = [],
            _instanceAppExpression = e
          }
  ExpressionIden (IdenAxiom n) ->
    Just $
      InstanceParamApp $
        InstanceApp
          { _instanceAppHead = n,
            _instanceAppArgs = [],
            _instanceAppExpression = e
          }
  ExpressionIden (IdenVar v)
    | HashSet.member v metaVars -> Just $ InstanceParamMeta v
    | otherwise -> Just $ InstanceParamVar v
  ExpressionHole h -> Just $ InstanceParamHole h
  ExpressionApplication app -> do
    let (h, args) = unfoldApplication app
    args' <- mapM (paramFromExpression metaVars) args
    case h of
      ExpressionIden (IdenInductive n) ->
        return $
          InstanceParamApp $
            InstanceApp
              { _instanceAppHead = n,
                _instanceAppArgs = toList args',
                _instanceAppExpression = e
              }
      ExpressionIden (IdenAxiom n) ->
        return $
          InstanceParamApp $
            InstanceApp
              { _instanceAppHead = n,
                _instanceAppArgs = toList args',
                _instanceAppExpression = e
              }
      _ ->
        Nothing
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

traitFromExpression :: HashSet VarName -> Expression -> Maybe InstanceApp
traitFromExpression metaVars e = case paramFromExpression metaVars e of
  Just (InstanceParamApp app) -> Just app
  _ -> Nothing

instanceFromTypedIden :: TypedIden -> Maybe InstanceInfo
instanceFromTypedIden TypedIden {..} = do
  InstanceApp {..} <- traitFromExpression metaVars e
  return $
    InstanceInfo
      { _instanceInfoInductive = _instanceAppHead,
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
