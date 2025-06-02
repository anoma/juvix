module Juvix.Compiler.Core.Transformation.DetectConstantSideConditions
  ( detectConstantSideConditions,
  )
where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base

detectConstantSideConditions :: forall r. (Members '[Error CoreError, Reader CoreOptions] r) => Module -> Sem r Module
detectConstantSideConditions md = mapAllNodesM (umapM go) md
  where
    mockFile = $(mkAbsFile "/detect-constant-side-conditions")
    defaultLoc = singletonInterval (mkInitialLoc mockFile)

    boolSym = lookupConstructorInfo md (BuiltinTag TagTrue) ^. constructorInductive

    go :: Node -> Sem r Node
    go node = case node of
      NMatch m -> NMatch <$> (overM matchBranches (mapMaybeM convertMatchBranch) m)
      _ -> return node

    convertMatchBranch :: MatchBranch -> Sem r (Maybe MatchBranch)
    convertMatchBranch br@MatchBranch {..} =
      case _matchBranchRhs of
        MatchBranchRhsExpression {} ->
          return $ Just br
        MatchBranchRhsIfs ifs ->
          case ifs1 of
            [] ->
              case nonEmpty ifs0 of
                Nothing -> return Nothing
                Just ifs0' -> return $ Just $ set matchBranchRhs (MatchBranchRhsIfs ifs0') br
            SideIfBranch {..} : ifs1' -> do
              fCoverage <- asks (^. optCheckCoverage)
              when (fCoverage && not (null ifs1'))
                $ throw
                  CoreError
                    { _coreErrorMsg = "Redundant side condition",
                      _coreErrorNode = Nothing,
                      _coreErrorLoc = fromMaybe defaultLoc (getInfoLocation (head' ifs1' ^. sideIfBranchInfo))
                    }
              let ifsBody = mkIfs boolSym (map (\(SideIfBranch i c b) -> (i, c, b)) ifs0) _sideIfBranchBody
              return $ Just $ set matchBranchRhs (MatchBranchRhsExpression ifsBody) br
          where
            ifs' = filter (not . isFalseConstr . (^. sideIfBranchCondition)) (toList ifs)
            (ifs0, ifs1) = span (not . isTrueConstr . (^. sideIfBranchCondition)) ifs'
