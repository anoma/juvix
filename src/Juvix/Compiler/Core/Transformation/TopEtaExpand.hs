module Juvix.Compiler.Core.Transformation.TopEtaExpand where

import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo qualified as Info
import Juvix.Compiler.Core.Transformation.Base

topEtaExpand :: InfoTable -> InfoTable
topEtaExpand info = run (mapT' go info)
  where
    go :: Symbol -> Node -> Sem '[InfoTableBuilder] Node
    go sym body = case info ^. infoIdentifiers . at sym of
      Nothing -> return body
      Just idenInfo ->
        let args :: [PiLhs]
            args = fst (unfoldPi (idenInfo ^. identifierType))
         in skipLambdas args body
      where
        skipLambdas :: [PiLhs] -> Node -> Sem '[InfoTableBuilder] Node
        skipLambdas args node = case args of
          [] -> return node
          (_ : as) -> case node of
            NLam l -> NLam <$> traverseOf lambdaBody (skipLambdas as) l
            _ -> do
              let newArgsInfo :: [ArgumentInfo]
                  newArgsInfo = map toArgumentInfo as
              overIdentArgsInfo sym (++ newArgsInfo)
              return (expand node (reverse args))
        toArgumentInfo :: PiLhs -> ArgumentInfo
        toArgumentInfo pi =
          ArgumentInfo
            { _argumentName = Info.getInfoName (pi ^. piLhsInfo),
              _argumentType = pi ^. piLhsBinder . binderType,
              _argumentIsImplicit = Explicit
            }
        expand :: Node -> [PiLhs] -> Node
        expand n = \case
          [] -> n
          (a : as) -> expand (reLambda (lambdaFromPi a) (mkApp' n (mkVar' 0))) as
        -- We keep the name and type. We drop the other info
        lambdaFromPi :: PiLhs -> LambdaLhs
        lambdaFromPi pi = LambdaLhs mempty (pi ^. piLhsBinder)
