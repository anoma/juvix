-- An optimizing transformation that folds lets whose values are immediate,
-- i.e., they don't require evaluation or memory allocation (variables or
-- constants), or when the bound variable occurs at most once in the body.
--
-- For example, transforms
-- ```
-- let x := y in let z := x + x in let u := z + y in x * x + z * z + u
-- ```
-- to
-- ```
-- let z := y + y in y * y + z * z + z + y
-- ```
module Juvix.Compiler.Core.Transformation.Optimize.LetFolding (letFolding, letFolding') where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.FreeVarsInfo as Info
import Juvix.Compiler.Core.Transformation.Base

convertNode :: (Node -> Bool) -> InfoTable -> Node -> Node
convertNode isFoldable tab = rmap go
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur = \case
      NLet Let {..}
        | isImmediate tab (_letItem ^. letItemValue)
            || isVarApp _letBody
            || Info.freeVarOccurrences 0 _letBody <= 1
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
letFolding' isFoldable tab =
  mapAllNodes
    ( removeInfo kFreeVarsInfo
        . convertNode isFoldable tab
        . computeFreeVarsInfo
    )
    tab

letFolding :: InfoTable -> InfoTable
letFolding = letFolding' (const False)
