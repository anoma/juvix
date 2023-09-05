module Juvix.Compiler.Internal.Data.InstanceInfo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data InstanceParam
  = InstanceParamVar VarName
  | InstanceParamApp InstanceApp
  | InstanceParamHole Hole
  | InstanceParamMeta VarName
  deriving stock (Eq)

data InstanceApp = InstanceApp
  { _instanceAppHead :: Name,
    _instanceAppArgs :: [InstanceParam],
    -- | The original expression from which this InstanceApp was created
    _instanceAppExpression :: Expression
  }
  deriving stock (Eq)

data InstanceInfo = InstanceInfo
  { _instanceInfoInductive :: InductiveName,
    _instanceInfoParams :: [InstanceParam],
    _instanceInfoResult :: Expression,
    _instanceInfoArgs :: [FunctionParameter]
  }

-- | Maps trait names to available instances
type InstanceTable = HashMap InductiveName [InstanceInfo]

makeLenses ''InstanceApp
makeLenses ''InstanceInfo

updateInstanceTable :: InstanceTable -> InstanceInfo -> InstanceTable
updateInstanceTable tab ii@InstanceInfo {..} =
  HashMap.alter go _instanceInfoInductive tab
  where
    go :: Maybe [InstanceInfo] -> Maybe [InstanceInfo]
    go = \case
      Just is -> Just (ii : is)
      Nothing -> Just [ii]

paramToExpression :: InstanceParam -> Expression
paramToExpression = \case
  InstanceParamVar v ->
    ExpressionIden (IdenVar v)
  InstanceParamApp (InstanceApp {..}) ->
    _instanceAppExpression
  InstanceParamHole h -> ExpressionHole h
  InstanceParamMeta {} ->
    impossible

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
  _ ->
    Nothing

traitFromExpression :: HashSet VarName -> Expression -> Maybe InstanceApp
traitFromExpression metaVars e = case paramFromExpression metaVars e of
  Just (InstanceParamApp app) -> Just app
  _ -> Nothing

instanceFromTypedExpression :: TypedExpression -> Maybe InstanceInfo
instanceFromTypedExpression TypedExpression {..} = case traitFromExpression metaVars e of
  Just (InstanceApp {..}) ->
    Just $
      InstanceInfo
        { _instanceInfoInductive = _instanceAppHead,
          _instanceInfoParams = _instanceAppArgs,
          _instanceInfoResult = _typedExpression,
          _instanceInfoArgs = args
        }
  Nothing -> Nothing
  where
    (args, e) = unfoldFunType _typedType
    metaVars = HashSet.fromList $ mapMaybe (^. paramName) args

checkNoMeta :: InstanceParam -> Bool
checkNoMeta = \case
  InstanceParamVar {} -> True
  InstanceParamMeta {} -> False
  InstanceParamHole {} -> True
  InstanceParamApp InstanceApp {..} -> all checkNoMeta _instanceAppArgs
