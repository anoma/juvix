-- An optimizing transformation that folds lets whose values are immediate,
-- i.e., they don't require evaluation or memory allocation (variables or
-- constants).
--
-- For example, transforms
-- ```
-- let x := y in let z := x + x in x + z
-- ```
-- to
-- ```
-- let z := y + y in y + z
-- ```
module Juvix.Compiler.Core.Transformation.Optimize.LetFolding where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: (Node -> Bool) -> InfoTable -> Node -> Node
convertNode isFoldable tab = rmap go
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur = \case
      NLet Let {..}
        | isImmediate tab (_letItem ^. letItemValue)
            || isVarApp _letBody
            || isFoldable (_letItem ^. letItemValue) ->
            go (recur . (mkBCRemove (_letItem ^. letItemBinder) val' :)) _letBody
        where
          val' = go recur (_letItem ^. letItemValue)
      node ->
        recur [] node

    isVarApp :: Node -> Bool
    isVarApp node =
      let (h, _) = unfoldApps' node
       in h == mkVar' 0

letFolding' :: (Node -> Bool) -> InfoTable -> InfoTable
letFolding' isFoldable tab = mapAllNodes (convertNode isFoldable tab) tab

letFolding :: InfoTable -> InfoTable
letFolding = letFolding' (const False)
