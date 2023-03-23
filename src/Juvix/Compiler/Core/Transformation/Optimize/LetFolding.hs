module Juvix.Compiler.Core.Transformation.Optimize.LetFolding where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Node -> Node
convertNode = rmap go
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur = \case
      NLet Let {..}
        | isImmediate (_letItem ^. letItemValue) ->
            go (recur . (mkBCRemove (_letItem ^. letItemBinder) val' :)) _letBody
        where
          val' = go recur (_letItem ^. letItemValue)
      node ->
        recur [] node

letFolding :: InfoTable -> InfoTable
letFolding = mapAllNodes convertNode
