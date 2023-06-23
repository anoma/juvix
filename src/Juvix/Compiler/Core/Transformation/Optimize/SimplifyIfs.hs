module Juvix.Compiler.Core.Transformation.Optimize.SimplifyIfs (simplifyIfs) where

import Data.List qualified as List
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Node -> Node
convertNode = umap go
  where
    go :: Node -> Node
    go node = case node of
      NCase Case {..}
        | isCaseBoolean _caseBranches
            && all (== List.head bodies) (List.tail bodies) ->
            List.head bodies
        where
          bodies = map (^. caseBranchBody) _caseBranches ++ maybeToList _caseDefault
      _ -> node

simplifyIfs :: InfoTable -> InfoTable
simplifyIfs = mapAllNodes convertNode
