module Juvix.Compiler.Tree.Transformation.Apply where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Extra.Apply
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Transformation.Base

computeFunctionApply :: ApplyBuiltins -> Node -> Node
computeFunctionApply blts = umap go
  where
    go :: Node -> Node
    go = \case
      CallClosures NodeCallClosures {..} -> goApply _nodeCallClosuresFun _nodeCallClosuresArgs
      node -> node

    -- TODO: NonEmpty instead of list
    goApply :: Node -> [Node] -> Node
    goApply cl args
      | n <= m = mkApply cl args
      | otherwise = goApply (mkApply cl (take m args)) (drop m args)
      where
        n = length args
        m = blts ^. applyBuiltinsNum

    mkApply :: Node -> [Node] -> Node
    mkApply cl args =
      Call
        NodeCall
          { _nodeCallType = CallFun sym,
            _nodeCallArgs = cl : args
          }
      where
        sym = fromJust $ HashMap.lookup (length args) (blts ^. applyBuiltinsMap)

computeApply :: InfoTable -> InfoTable
computeApply tab = mapT (const (computeFunctionApply blts)) tab'
  where
    (blts, tab') = addApplyBuiltins tab

checkNoCallClosures :: InfoTable -> Bool
checkNoCallClosures tab =
  all (ufold (\b bs -> b && and bs) go . (^. functionCode)) (tab ^. infoFunctions)
  where
    go :: Node -> Bool
    go = \case
      CallClosures {} -> False
      _ -> True
