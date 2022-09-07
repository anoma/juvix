module Juvix.Compiler.Core.Transformation.LambdaLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Info.TypeInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

lambdaLiftNode :: forall r. Member InfoTableBuilder r => BinderList Info -> Node -> Sem r Node
lambdaLiftNode aboveBl top = mkLambdas topArgs <$> dmapLRM' (topArgsBinderList <> aboveBl, go) body
  where
    (topArgs, body) = unfoldLambdas top
    topArgsBinderList :: BinderList Info
    topArgsBinderList = BL.fromList topArgs
    typeFromArgs :: [ArgumentInfo] -> Type
    typeFromArgs = \case
      [] -> mkDynamic' -- change this when we have type info about the body
      (a : as) -> mkPi' argTy (typeFromArgs as)
        where
          argTy = fromMaybe mkDynamic' (a ^. argumentType)
    -- extracts the argument info from the binder
    argInfo :: Info -> ArgumentInfo
    argInfo i = ArgumentInfo {
      _argumentName = (^. infoName) <$> Info.lookup (Proxy @NameInfo) i,
      _argumentType = (^. infoType) <$> Info.lookup (Proxy @TypeInfo) i,
      _argumentIsImplicit = False
      }
    go :: BinderList Info -> Node -> Sem r Recur
    go bl = \case
      l@NLam {} -> do
        l' <- lambdaLiftNode bl l
        let freevars = toList (getFreeVars l')
            freevarsAssocs :: [(Index, Info)]
            freevarsAssocs = [(i, BL.lookup i bl) | i <- map (^. varIndex) freevars]
            fBody' = captureFreeVars freevarsAssocs l'
        f <- freshSymbol
        let argsInfo :: [ArgumentInfo]
            argsInfo = map (argInfo . snd) freevarsAssocs
        registerIdent IdentifierInfo {
          _identifierSymbol = f,
          _identifierName = Nothing,
          _identifierType = typeFromArgs argsInfo,
          _identifierArgsNum = length freevars,
          _identifierArgsInfo = argsInfo,
          _identifierIsExported = False
          }
        registerIdentNode f fBody'
        let fApp = mkApps' (mkIdent mempty f) (map NVar freevars)
        return (End fApp)
      m -> return (Recur m)

lambdaLifting :: InfoTable -> InfoTable
lambdaLifting = run . mapT' (lambdaLiftNode mempty)

-- | True if lambdas are only found at the top level.
isLifted :: Node -> Bool
isLifted = not . has (cosmos . _NLam) . snd . unfoldLambdas'
