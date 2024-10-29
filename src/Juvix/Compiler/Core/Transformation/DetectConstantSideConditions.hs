module Juvix.Compiler.Core.Transformation.DetectConstantSideConditions
  ( detectConstantSideConditions,
  )
where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

detectConstantSideConditions :: Module -> Module
detectConstantSideConditions md = mapAllNodes (umap go) md
  where
    boolSym = lookupConstructorInfo md (BuiltinTag TagTrue) ^. constructorInductive

    go :: Node -> Node
    go node = case node of
      NMatch m -> NMatch (over matchBranches (concatMap convertMatchBranch) m)
      _ -> node

    convertMatchBranch :: MatchBranch -> [MatchBranch]
    convertMatchBranch br@MatchBranch {..} =
      case _matchBranchRhs of
        MatchBranchRhsExpression {} ->
          [br]
        MatchBranchRhsIfs ifs ->
          case ifs1 of
            [] ->
              case nonEmpty ifs0 of
                Nothing -> []
                Just ifs0' -> [set matchBranchRhs (MatchBranchRhsIfs ifs0') br]
            SideIfBranch {..} : ifs1' ->
              let ifsBody = mkIfs boolSym (map (\(SideIfBranch i c b) -> (i, c, b)) ifs0) _sideIfBranchBody
               in set matchBranchRhs (MatchBranchRhsExpression ifsBody) br
                    :
                    -- All branches after the first true branch are redundant
                    -- and can be removed. We leave one of the redundant
                    -- branches to make redundant pattern detection work in this
                    -- case.
                    case ifs1' of
                      [] -> []
                      if1 : _ -> [set matchBranchRhs (MatchBranchRhsExpression (if1 ^. sideIfBranchBody)) br]
          where
            ifs' = filter (not . isFalseConstr . (^. sideIfBranchCondition)) (toList ifs)
            (ifs0, ifs1) = span (not . isTrueConstr . (^. sideIfBranchCondition)) ifs'
