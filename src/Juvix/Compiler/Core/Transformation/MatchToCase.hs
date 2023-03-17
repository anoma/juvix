module Juvix.Compiler.Core.Transformation.MatchToCase where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

data PatternRow = PatternRow
  { _patternRowPatterns :: [Pattern],
    _patternRowBody :: Node,
    -- | The number of initial wildcard binders in `_patternRowPatterns` which
    -- don't originate from the input
    _patternRowIgnoredPatternsNum :: Int,
    -- | Previous binder changes, reversed
    _patternRowBinderChangesRev :: [BinderChange]
  }

makeLenses ''PatternRow

type PatternMatrix = [PatternRow]

-- | Compiles pattern matches (`Match` nodes) to decision trees built up from
-- `Case` nodes. The algorithm is based on the paper: Luc Maranget, "Compiling
-- Pattern Matching to Good Decision Trees", ML'08.
matchToCase :: InfoTable -> InfoTable
matchToCase tab = mapAllNodes (rmap (goMatchToCase tab)) tab

goMatchToCase :: InfoTable -> ([BinderChange] -> Node -> Node) -> Node -> Node
goMatchToCase tab recur = \case
  NMatch m ->
    compileMatch m
  node ->
    recur [] node
  where
    compileMatch :: Match -> Node
    compileMatch Match {..} =
      go 0 (zipExact (toList _matchValues) (toList _matchValueTypes))
      where
        go :: Int -> [(Node, Type)] -> Node
        go n = \case
          [] ->
            compile err n [0 .. n - 1] matrix
            where
              err = hsep . take n
              matrix = map matchBranchToPatternRow _matchBranches

              matchBranchToPatternRow :: MatchBranch -> PatternRow
              matchBranchToPatternRow MatchBranch {..} =
                PatternRow
                  { _patternRowPatterns = toList _matchBranchPatterns,
                    _patternRowBody = _matchBranchBody,
                    _patternRowIgnoredPatternsNum = 0,
                    _patternRowBinderChangesRev = [BCAdd n]
                  }
          (val, valty) : vs' ->
            mkLet'
              (goMatchToCase tab (recur . (BCAdd n :)) valty)
              (goMatchToCase tab (recur . (BCAdd n :)) val)
              (go (n + 1) vs')

    -- `compile err bindersNum vs matrix`:
    --  - `err` creates a textual representation of an unmatched pattern
    --    sequence, given as arguments the representations of patterns for the
    --    holes (corresponding to the matched values `vs`)
    --  - `bindersNum` is the number of binders added so far
    --  - `vs` are the de Bruijn levels of the values matched on w.r.t. the
    --    first added binder; a value matched on is always a variable referring
    --    to one of the binders added so far
    --  - `matrix` is the pattern matching matrix
    compile :: ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> PatternMatrix -> Node
    compile err bindersNum vs matrix = case matrix of
      [] ->
        -- The matrix has no rows -- matching fails (Section 4, case 1).
        error $ "Pattern matching not exhaustive. Example pattern sequence not matched: " <> show (ppOutput $ err (repeat "_"))
      r@PatternRow {..} : _
        | all isPatWildcard _patternRowPatterns ->
            -- The first row matches all values (Section 4, case 2)
            compileMatchingRow bindersNum vs r
      _ ->
        -- Section 4, case 3
        -- Select the first column
        let vl = List.head vs
            vs' = List.tail vs
            val = mkVal bindersNum vl
            (col, matrix') = decompose val matrix
            tagsSet = getPatTags col
            tags = toList tagsSet
            ind = fromJust (HashMap.lookup (List.head tags) (tab ^. infoConstructors)) ^. constructorInductive
            ctrsNum = length (fromJust (HashMap.lookup ind (tab ^. infoInductives)) ^. inductiveConstructors)
         in if
                | null tags ->
                    -- There are no constructor patterns
                    compileDefault (missingTag ind tagsSet) err bindersNum vs' col matrix'
                | otherwise ->
                    -- Section 4, case 3(a)
                    NCase
                      Case
                        { _caseInfo = mempty,
                          _caseInductive = ind,
                          _caseValue = val,
                          _caseBranches = map (compileBranch err bindersNum vs' col matrix') tags,
                          _caseDefault =
                            if
                                | length tags == ctrsNum ->
                                    Nothing
                                | otherwise ->
                                    Just $ compileDefault (missingTag ind tagsSet) err bindersNum vs' col matrix'
                        }

    missingTag :: Symbol -> HashSet Tag -> Tag
    missingTag ind tags = fromJust $ find (not . flip HashSet.member tags) (map (^. constructorTag) (ii ^. inductiveConstructors))
      where
        ii = fromJust $ HashMap.lookup ind (tab ^. infoInductives)

    mkVal :: Level -> Level -> Node
    mkVal bindersNum vl = mkVar' (getBinderIndex bindersNum vl)

    decompose :: Node -> PatternMatrix -> ([Pattern], PatternMatrix)
    decompose val matrix = (col, matrix')
      where
        col = map (List.head . (^. patternRowPatterns)) matrix
        matrix' = map updateRow matrix
        binder = getPatternBinder (List.head col)

        updateRow :: PatternRow -> PatternRow
        updateRow row =
          row
            { _patternRowPatterns = List.tail (row ^. patternRowPatterns),
              _patternRowIgnoredPatternsNum = max 0 (nIgnored - 1),
              _patternRowBinderChangesRev =
                if
                    | nIgnored > 0 -> rbcs
                    | otherwise -> BCRemove (BinderRemove binder val) : rbcs
            }
          where
            nIgnored = row ^. patternRowIgnoredPatternsNum
            rbcs = row ^. patternRowBinderChangesRev

    getPatTags :: [Pattern] -> HashSet Tag
    getPatTags = \case
      [] ->
        mempty
      PatConstr PatternConstr {..} : pats ->
        HashSet.insert _patternConstrTag (getPatTags pats)
      _ : pats ->
        getPatTags pats

    compileMatchingRow :: Level -> [Level] -> PatternRow -> Node
    compileMatchingRow bindersNum vs PatternRow {..} =
      goMatchToCase tab (recur . (bcs ++)) _patternRowBody
      where
        bcs =
          reverse $
            foldl'
              ( \acc (pat, vl) ->
                  BCRemove (BinderRemove (getPatternBinder pat) (mkVal bindersNum vl)) : acc
              )
              _patternRowBinderChangesRev
              (drop _patternRowIgnoredPatternsNum (zipExact _patternRowPatterns vs))

    -- `compileBranch` computes S(c, M) where `c = Constr tag` and `M =
    -- col:matrix`, as described in Section 2, Figure 1 in the paper. Then it
    -- continues compilation with the new matrix.
    compileBranch :: ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Tag -> CaseBranch
    compileBranch err bindersNum vs col matrix tag =
      CaseBranch
        { _caseBranchInfo = setInfoName (ci ^. constructorName) mempty,
          _caseBranchTag = tag,
          _caseBranchBinders = map mkBinder' argtys,
          _caseBranchBindersNum = argsNum,
          _caseBranchBody = compile err' bindersNum' (vs' ++ vs) matrix'
        }
      where
        ci = fromJust $ HashMap.lookup tag (tab ^. infoConstructors)
        argtys = typeArgs $ fromJust (HashMap.lookup tag (tab ^. infoConstructors)) ^. constructorType
        argsNum = length argtys
        bindersNum' = bindersNum + argsNum
        vs' = [bindersNum .. bindersNum + argsNum - 1]
        matrix' =
          catMaybes $
            zipWithExact
              ( \pat row ->
                  case pat of
                    PatConstr PatternConstr {..}
                      | _patternConstrTag == tag ->
                          Just $
                            row
                              { _patternRowPatterns =
                                  _patternConstrArgs ++ row ^. patternRowPatterns,
                                _patternRowBinderChangesRev = BCAdd argsNum : row ^. patternRowBinderChangesRev
                              }
                    PatWildcard {} ->
                      Just $
                        row
                          { _patternRowPatterns =
                              map (PatWildcard . PatternWildcard mempty . Binder "_" Nothing) argtys
                                ++ row ^. patternRowPatterns,
                            _patternRowBinderChangesRev = BCAdd argsNum : row ^. patternRowBinderChangesRev,
                            _patternRowIgnoredPatternsNum = argsNum + row ^. patternRowIgnoredPatternsNum
                          }
                    _ ->
                      Nothing
              )
              col
              matrix
        err' args =
          err
            (parensIf (argsNum > paramsNum) (foldl' (<+>) (pretty (ci ^. constructorName)) (drop paramsNum (take argsNum args))) : drop argsNum args)
        paramsNum = getTypeParamsNum tab (ci ^. constructorType)

    -- `compileDefault` computes D(M) where `M = col:matrix`, as described in
    -- Section 2, Figure 1 in the paper. Then it continues compilation with the
    -- new matrix.
    compileDefault :: Tag -> ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Node
    compileDefault tag err bindersNum vs col matrix =
      compile err' bindersNum vs matrix'
      where
        matrix' =
          catMaybes $
            zipWithExact
              ( \pat row ->
                  case pat of
                    PatWildcard {} -> Just row
                    _ -> Nothing
              )
              col
              matrix
        err' args = err (parensIf (argsNum > 0) (pretty (ci ^. constructorName) <+> hsep (replicate argsNum "_")) : args)
        ci = fromJust $ HashMap.lookup tag (tab ^. infoConstructors)
        paramsNum = getTypeParamsNum tab (ci ^. constructorType)
        argsNum = ci ^. constructorArgsNum - paramsNum
