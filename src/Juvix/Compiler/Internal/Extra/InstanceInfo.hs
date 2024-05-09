module Juvix.Compiler.Internal.Extra.InstanceInfo
  ( module Juvix.Compiler.Internal.Data.InstanceInfo,
    module Juvix.Compiler.Internal.Extra.InstanceInfo,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Store.Internal.Data.FunctionsTable
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

paramFromExpression :: FunctionsTable -> HashSet VarName -> Expression -> Maybe InstanceParam
paramFromExpression funtab metaVars e = case e of
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
  ExpressionIden (IdenFunction n) ->
    case HashMap.lookup n (funtab ^. functionsTable) of
      Just def -> paramFromExpression funtab metaVars def
      Nothing -> Nothing
  ExpressionHole h -> Just $ InstanceParamHole h
  ExpressionApplication app -> do
    let (h, args) = unfoldApplication app
    args' <- mapM (paramFromExpression funtab metaVars) args
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
      ExpressionIden (IdenFunction n) ->
        case HashMap.lookup n (funtab ^. functionsTable) of
          Just def -> case paramFromExpression funtab metaVars def of
            Just (InstanceParamApp InstanceApp {..}) ->
              return $
                InstanceParamApp $
                  InstanceApp
                    { _instanceAppHead,
                      _instanceAppArgs = _instanceAppArgs ++ toList args',
                      _instanceAppExpression = e
                    }
            _ -> Nothing
          Nothing -> Nothing
      _ ->
        Nothing
  ExpressionFunction Function {..}
    | _functionLeft ^. paramImplicit == Explicit -> do
        l <- paramFromExpression funtab metaVars (_functionLeft ^. paramType)
        r <- paramFromExpression funtab metaVars _functionRight
        return $
          InstanceParamFun
            InstanceFun
              { _instanceFunLeft = l,
                _instanceFunRight = r,
                _instanceFunExpression = e
              }
  _ ->
    Nothing

traitFromExpression :: FunctionsTable -> HashSet VarName -> Expression -> Maybe InstanceApp
traitFromExpression funtab metaVars e = case paramFromExpression funtab metaVars e of
  Just (InstanceParamApp app) -> Just app
  _ -> Nothing

instanceFromTypedExpression :: FunctionsTable -> TypedExpression -> Maybe InstanceInfo
instanceFromTypedExpression funtab TypedExpression {..} = do
  InstanceApp {..} <- traitFromExpression funtab metaVars e
  return $
    InstanceInfo
      { _instanceInfoInductive = _instanceAppHead,
        _instanceInfoParams = _instanceAppArgs,
        _instanceInfoResult = _typedExpression,
        _instanceInfoArgs = args
      }
  where
    (args, e) = unfoldFunType _typedType
    metaVars = HashSet.fromList $ mapMaybe (^. paramName) args

checkNoMeta :: InstanceParam -> Bool
checkNoMeta = \case
  InstanceParamVar {} -> True
  InstanceParamMeta {} -> False
  InstanceParamHole {} -> True
  InstanceParamApp InstanceApp {..} -> all checkNoMeta _instanceAppArgs
  InstanceParamFun InstanceFun {..} ->
    checkNoMeta _instanceFunLeft && checkNoMeta _instanceFunRight
