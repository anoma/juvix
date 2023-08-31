module Juvix.Compiler.Internal.Data.InstanceInfo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data InstanceParam
  = InstanceParamVar InstanceVar
  | InstanceParamApp InstanceApp
  | InstanceParamMeta InstanceMeta
  deriving stock (Eq)

newtype InstanceVar = InstanceVar
  { _instanceVarName :: VarName
  }
  deriving stock (Eq)

newtype InstanceMeta = InstanceMeta
  { _instanceMetaName :: VarName
  }
  deriving stock (Eq)

data InstanceApp = InstanceApp
  { _instanceAppHead :: InductiveName,
    _instanceAppArgs :: [InstanceParam]
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

makeLenses ''InstanceVar
makeLenses ''InstanceApp
makeLenses ''InstanceMeta
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
  InstanceParamVar (InstanceVar v) ->
    ExpressionIden (IdenVar v)
  InstanceParamApp (InstanceApp h args) ->
    foldExplicitApplication (ExpressionIden (IdenInductive h)) (map paramToExpression args)
  InstanceParamMeta {} ->
    impossible

paramFromExpression :: HashSet VarName -> Expression -> Maybe InstanceParam
paramFromExpression metaVars = \case
  ExpressionIden (IdenInductive n) ->
    Just $ InstanceParamApp $ InstanceApp n []
  ExpressionIden (IdenVar v)
    | HashSet.member v metaVars -> Just $ InstanceParamMeta $ InstanceMeta v
    | otherwise -> Just $ InstanceParamVar $ InstanceVar v
  ExpressionApplication app -> do
    let (h, args) = unfoldApplication app
    args' <- mapM (paramFromExpression metaVars) args
    case h of
      ExpressionIden (IdenInductive n) ->
        return $ InstanceParamApp $ InstanceApp n (toList args')
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
    metaVars = HashSet.fromList $ mapMaybe (^. paramName) args
