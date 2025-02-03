module Juvix.Compiler.Core.Transformation.DetectRedundantPatterns where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pretty hiding (Options)
import Juvix.Compiler.Core.Transformation.Base

type PatternRow = [Pattern]

type PatternMatrix = [PatternRow]

-- | Checks for redundant patterns in `Match` nodes. The algorithm is based on
-- the paper: Luc Maranget, "Warnings for pattern matching", JFP 17 (3):
-- 387–421, 2007.
detectRedundantPatterns :: (Members '[Error CoreError, Reader CoreOptions] r) => Module -> Sem r Module
detectRedundantPatterns md = do
  fCoverage <- asks (^. optCheckCoverage)
  if
      | fCoverage ->
          mapAllNodesM (umapM (goDetectRedundantPatterns md)) md
      | otherwise ->
          return md

goDetectRedundantPatterns ::
  forall r.
  (Members '[Error CoreError, Reader CoreOptions] r) =>
  Module ->
  Node ->
  Sem r Node
goDetectRedundantPatterns md node = case node of
  NMatch m -> do
    checkMatch m
    return node
  _ -> return node
  where
    mockFile = $(mkAbsFile "/detect-redundant-patterns")
    defaultLoc = singletonInterval (mkInitialLoc mockFile)

    checkMatch :: Match -> Sem r ()
    checkMatch Match {..} = case dropWhile isMatchBranchRhsIf $ _matchBranches of
      [] -> return ()
      MatchBranch {..} : brs -> go [toList _matchBranchPatterns] brs
      where
        go :: PatternMatrix -> [MatchBranch] -> Sem r ()
        go matrix = \case
          [] -> return ()
          MatchBranch {..} : branches -> do
            let row = toList _matchBranchPatterns
            unless (check matrix row) $
              throw
                CoreError
                  { _coreErrorMsg = ppOutput ("Redundant pattern" <> seq <> ": " <> pat <> "\nPerhaps you mistyped a constructor name in an earlier pattern?"),
                    _coreErrorNode = Nothing,
                    _coreErrorLoc = fromMaybe defaultLoc (getInfoLocation _matchBranchInfo)
                  }
            case _matchBranchRhs of
              MatchBranchRhsExpression {} -> go (row : matrix) branches
              MatchBranchRhsIfs {} -> go matrix branches
            where
              opts = defaultOptions {_optPrettyPatterns = True}
              seq = if isSingleton (toList _matchBranchPatterns) then "" else " sequence"
              pat = if isSingleton (toList _matchBranchPatterns) then doc opts (head _matchBranchPatterns) else docSequence opts (toList _matchBranchPatterns)

    -- Returns True if vector is useful (not redundant) for matrix, i.e. it is
    -- not covered by any row in the matrix. See Definition 6 and Section 3.1 in
    -- the paper.
    check :: PatternMatrix -> PatternRow -> Bool
    check matrix vector = case vector of
      []
        | null matrix -> True
        | otherwise -> False
      (p : ps) -> case p of
        PatConstr PatternConstr {..} ->
          check
            (specialize _patternConstrTag (length _patternConstrArgs) matrix)
            (_patternConstrArgs ++ ps)
        PatWildcard {} ->
          let col = map head' matrix
              tagsSet = getPatTags col
              tags = toList tagsSet
              ind = lookupConstructorInfo md (head' tags) ^. constructorInductive
              ctrsNum = length (lookupInductiveInfo md ind ^. inductiveConstructors)
           in if
                  | not (null tags) && length tags == ctrsNum ->
                      go tags
                  | otherwise ->
                      check (computeDefault matrix) ps
          where
            go :: [Tag] -> Bool
            go = \case
              [] -> False
              (tag : tags') ->
                check matrix' (replicate argsNum p ++ ps) || go tags'
                where
                  argsNum = lookupConstructorInfo md tag ^. constructorArgsNum
                  matrix' = specialize tag argsNum matrix

    getPatTags :: [Pattern] -> HashSet Tag
    getPatTags = \case
      [] ->
        mempty
      PatConstr PatternConstr {..} : pats ->
        HashSet.insert _patternConstrTag (getPatTags pats)
      _ : pats ->
        getPatTags pats

    specialize :: Tag -> Int -> PatternMatrix -> PatternMatrix
    specialize tag argsNum = mapMaybe go
      where
        go :: PatternRow -> Maybe PatternRow
        go row = case row of
          PatConstr PatternConstr {..} : row'
            | _patternConstrTag == tag -> Just $ _patternConstrArgs ++ row'
            | otherwise -> Nothing
          w@PatWildcard {} : row' ->
            Just $ replicate argsNum w ++ row'
          [] -> impossible

    computeDefault :: PatternMatrix -> PatternMatrix
    computeDefault matrix = mapMaybe go matrix
      where
        go :: PatternRow -> Maybe PatternRow
        go row = case row of
          PatConstr {} : _ -> Nothing
          PatWildcard {} : row' -> Just row'
          [] -> impossible
