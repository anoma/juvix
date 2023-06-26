module Juvix.Compiler.Core.Transformation.Optimize.CaseFolding (caseFolding) where

import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Node -> Node
convertNode = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NCase Case {..} ->
        case _caseValue of
          NCtr Constr {..} ->
            case find ((== _constrTag) . (^. caseBranchTag)) _caseBranches of
              Just CaseBranch {..} ->
                goBranch 0 _caseBranchBinders _constrArgs _caseBranchBody
              Nothing -> node
          _ -> node
      _ -> node

    goBranch :: Int -> [Binder] -> [Node] -> Node -> Node
    goBranch k bs args body = case (bs, args) of
      ([], []) ->
        body
      (b : bs', arg : args') ->
        mkLet mempty b (shift k arg) (goBranch (k + 1) bs' args' body)
      _ ->
        impossible

caseFolding :: InfoTable -> InfoTable
caseFolding = mapAllNodes convertNode
