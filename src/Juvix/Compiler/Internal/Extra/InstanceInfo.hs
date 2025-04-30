module Juvix.Compiler.Internal.Extra.InstanceInfo
  ( module Juvix.Compiler.Store.Internal.Data.InstanceInfo,
    module Juvix.Compiler.Internal.Extra.InstanceInfo,
    module Juvix.Compiler.Internal.Data.TypedIden,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Data.TypedIden
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
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

paramFromExpression :: forall r. (Members '[Reader BuiltinsTable] r) => HashSet VarName -> Expression -> Sem (Fail ': r) InstanceParam
paramFromExpression metaVars e = case e of
  ExpressionIden (IdenVar v)
    | HashSet.member v metaVars -> return (InstanceParamMeta v)
    | otherwise -> return (InstanceParamVar v)
  ExpressionIden i -> do
    h <- mkInstanceAppHeadIden i
    return $
      InstanceParamApp $
        InstanceApp
          { _instanceAppHead = h,
            _instanceAppArgs = [],
            _instanceAppExpression = e
          }
  ExpressionHole h -> return (InstanceParamHole h)
  ExpressionApplication app ->
    failAlts
      [ builtinApplication app,
        normalApplication app
      ]
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
  _ -> fail
  where
    builtinApplication :: Application -> Sem (Fail ': r) InstanceParam
    builtinApplication = builtinNatApplication

    builtinNatApplication :: Application -> Sem (Fail ': r) InstanceParam
    builtinNatApplication app = do
      (ExpressionIden (IdenFunction fromNat_), [argNat, _argNatI, argLit :: ApplicationArg]) <- return (second toList (unfoldApplication' app))
      matchBuiltinName BuiltinFromNat fromNat_
      ExpressionIden (IdenInductive nat_) <- return (argNat ^. appArg)
      matchBuiltinName BuiltinNat nat_
      let l :: Expression = argLit ^. appArg
      ExpressionLiteral (WithLoc _ (LitNatural num)) <- return l
      failWhen (num < 0)
      u <- (^. typedExpression) . fromRight impossible <$> runError @TypeCheckerError (unaryNatural (getLoc app) num)
      paramFromExpression metaVars u

    normalApplication :: Application -> Sem (Fail ': r) InstanceParam
    normalApplication app = do
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

    mkInstanceAppHeadIden :: forall r'. (Members '[Fail] r') => Iden -> Sem r' InstanceAppHead
    mkInstanceAppHeadIden = \case
      IdenInductive n -> return (InstanceAppHeadInductive n)
      IdenAxiom n -> return (InstanceAppHeadAxiom n)
      IdenConstructor n -> return (InstanceAppHeadConstructor n)
      _ -> fail

    mkInstanceAppHead :: forall r'. (Members '[Fail] r') => Expression -> Sem r' InstanceAppHead
    mkInstanceAppHead = \case
      ExpressionIden i -> mkInstanceAppHeadIden i
      _ -> fail

traitFromExpression :: (Members '[Reader BuiltinsTable] r) => HashSet VarName -> Expression -> Sem (Fail ': r) InstanceApp
traitFromExpression metaVars e = do
  x <- paramFromExpression metaVars e
  case x of
    InstanceParamApp app -> return app
    _ -> fail

instanceFromTypedIden :: (Members '[Reader BuiltinsTable] r) => TypedIden -> Sem (Fail ': r) InstanceInfo
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
