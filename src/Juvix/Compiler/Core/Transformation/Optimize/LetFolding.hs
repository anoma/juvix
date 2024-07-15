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

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.FreeVarsInfo as Info
import Juvix.Compiler.Core.Transformation.Base

convertNode :: (Module -> BinderList Binder -> Node -> Bool) -> Module -> Node -> Node
convertNode isFoldable md = rmapL go
  where
    go :: ([BinderChange] -> Node -> Node) -> BinderList Binder -> Node -> Node
    go recur bl = \case
      NLet Let {..}
        | isImmediate md (_letItem ^. letItemValue)
            || Info.freeVarOccurrences 0 _letBody
            <= 1
            || isFoldable md bl (_letItem ^. letItemValue) ->
            go (recur . (mkBCRemove b val' :)) (BL.cons b bl) _letBody
        where
          val' = go recur bl (_letItem ^. letItemValue)
          b = _letItem ^. letItemBinder
      node ->
        recur [] node

letFolding' :: (Module -> BinderList Binder -> Node -> Bool) -> Module -> Module
letFolding' isFoldable tab =
  mapAllNodes
    ( removeInfo kFreeVarsInfo
        . convertNode isFoldable tab
        . computeFreeVarsInfo
    )
    tab

letFolding :: Module -> Module
letFolding = letFolding' (\_ _ _ -> False)
