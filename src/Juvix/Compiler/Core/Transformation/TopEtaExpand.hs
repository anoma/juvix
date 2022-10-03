module Juvix.Compiler.Core.Transformation.TopEtaExpand where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

topEtaExpand :: InfoTable -> InfoTable
topEtaExpand info = mapT go info
  where
    go :: Symbol -> Node -> Node
    go sym node = case info ^. infoIdentifiers . at sym of
      Nothing -> node
      Just idenInfo ->
        let args :: [Info]
            args = map infoFromArgumentInfo (idenInfo ^. identifierArgsInfo)
         in skipLambdas args node
    skipLambdas :: [Info] -> Node -> Node
    skipLambdas args node = case args of
      [] -> node
      (_ : as) -> case node of
        NLam l -> NLam (over lambdaBody (skipLambdas as) l)
        _ -> expand node (reverse args)
    expand :: Node -> [Info] -> Node
    expand n = \case
      [] -> n
      (a : as) -> expand (mkLambda a (mkApp' n (mkVar' 0))) as
