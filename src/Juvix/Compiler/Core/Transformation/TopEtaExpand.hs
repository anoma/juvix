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
      let
        args :: [Info]
        args = map infoFromArgumentInfo  (idenInfo ^. identifierArgsInfo)
      in goArgs args node
  goArgs :: [Info] -> Node -> Node
  goArgs args node = case args of
    [] -> node
    (a : as) -> case node of
      NLam l -> NLam (over lambdaBody (goArgs as) l)
      _ -> expand node (reverse args)
  expand :: Node -> [Info] -> Node
  expand n = \case
    [] -> n
    (a : as) -> expand (mkLambda a (mkApp' n (mkVar' 0))) as
