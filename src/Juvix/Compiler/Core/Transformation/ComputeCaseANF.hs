module Juvix.Compiler.Core.Transformation.ComputeCaseANF (computeCaseANF) where

-- A transformation which lifts out non-immediate values matched on in case
-- expressions by introducing let-bindings for them. In essence, this is a
-- partial ANF transformation for case expressions only.
--
-- For example, transforms
-- ```
-- case f x of { c y := y + x; d y := y }
-- ```
-- to
-- ```
-- let z := f x in case z of { c y := y + x; d y := y }
-- ```

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.TypeInfo qualified as Info
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo (computeNodeTypeInfo)

convertNode :: Module -> Node -> Node
convertNode md = Info.removeTypeInfo . rmapL go . computeNodeTypeInfo md
  where
    go :: ([BinderChange] -> Node -> Node) -> BinderList Binder -> Node -> Node
    go recur bl node = case node of
      NCase Case {..}
        | not (isImmediate md _caseValue) ->
            mkLet _caseInfo b val' $
              NCase
                Case
                  { _caseValue = mkVar' 0,
                    _caseBranches = map goCaseBranch _caseBranches,
                    _caseDefault = fmap (go (recur . (BCAdd 1 :)) bl) _caseDefault,
                    _caseInfo,
                    _caseInductive
                  }
        where
          val' = go recur bl _caseValue
          b = Binder "case_value" Nothing ty
          ty = Info.getNodeType _caseValue

          goCaseBranch :: CaseBranch -> CaseBranch
          goCaseBranch CaseBranch {..} =
            CaseBranch
              { _caseBranchBody =
                  go
                    (recur . ((BCAdd 1 : map BCKeep _caseBranchBinders) ++))
                    (BL.prependRev _caseBranchBinders bl)
                    _caseBranchBody,
                _caseBranchTag,
                _caseBranchInfo,
                _caseBranchBindersNum,
                _caseBranchBinders
              }
      _ ->
        recur [] node

computeCaseANF :: Module -> Module
computeCaseANF md =
  mapAllNodes (convertNode md) md
