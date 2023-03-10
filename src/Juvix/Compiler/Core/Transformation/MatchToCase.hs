module Juvix.Compiler.Core.Transformation.MatchToCase where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

data PatternRow = PatternRow
  { _patternRowPatterns :: [Pattern],
    _patternRowBody :: Node
  }

makeLenses ''PatternRow

type PatternMatrix = [PatternRow]

-- | Compiles pattern matches (`Match` nodes) to decision trees built up from
-- `Case` nodes. The algorithm is based on the paper: Luc Maranget, "Compiling
-- Pattern Matching to Good Decision Trees", ML'08.
matchToCase :: InfoTable -> InfoTable
matchToCase tab = mapAllNodes (convert mempty 0) tab
  where
    convert :: HashMap Level Level -> Level -> Node -> Node
    convert levelMap bl node = dmapCNR' (bl, go) levelMap node

    go :: HashMap Level Level -> Level -> Node -> Recur' (HashMap Level Level)
    go levelMap bl = \case
      NVar (Var {..}) ->
        End' (mkVar _varInfo (adjustIndex levelMap bl _varIndex))
      NMatch (Match {..}) ->
        End' $
          snd $
            foldr
              ( \(val, valty) (blo, node) ->
                  ( blo - 1,
                    mkLet'
                      (shift (blo - bl) (convert levelMap bl valty))
                      (shift (blo - bl) (convert levelMap bl val))
                      node
                  )
              )
              ( bl + n - 1,
                compile err levelMap bl (bl + n) [bl .. bl + n - 1] matrix
              )
              (zipExact (toList _matchValues) (toList _matchValueTypes))
        where
          n = length _matchValues
          err = hsep . take n
          matrix = map matchBranchToPatternRow _matchBranches
      node ->
        Recur' (levelMap, node)

    matchBranchToPatternRow :: MatchBranch -> PatternRow
    matchBranchToPatternRow MatchBranch {..} =
      PatternRow
        { _patternRowPatterns = toList _matchBranchPatterns,
          _patternRowBody = _matchBranchBody
        }

    -- `compile err levels bl vls matrix`:
    --  - `err` creates a textual representation of an unmatched pattern
    --    sequence, given as arguments the representations of patterns for the
    --    holes (corresponding to the matched values `vs`)
    --  - `levelMap` maps input de Bruijn levels to output de Bruijn levels
    --  - `bli` is the current de Bruijn level in the input node
    --  - `blo` is the current de Bruijn level in the output node
    --  - `vs` are the de Bruijn levels of the values matched on, w.r.t. the
    --    output node
    --  - `matrix` is the pattern matching matrix
    --
    -- Hence, `blo - bli` is the number of extra binders inserted in the output
    -- node, in comparison to the current position in the input node.
    compile :: ([Doc Ann] -> Doc Ann) -> HashMap Level Level -> Level -> Level -> [Level] -> PatternMatrix -> Node
    compile err levelMap bli blo vs matrix = case matrix of
      [] ->
        -- The matrix has no rows -- matching fails (Section 4, case 1).
        error $ show $ ppOutput $ err (repeat "_")
      r@PatternRow {..} : _
        | all isPatWildcard _patternRowPatterns ->
            -- The first row matches all values (Section 4, case 2)
            compileMatchingRow levelMap bli blo vs r
      _ ->
        -- Section 4, case 3
        -- Select the first column
        let (col, matrix') = decompose matrix
            tags = toList (getPatTags col)
            vl = List.head vs
            vs' = List.tail vs
            val = mkVar' (getBinderIndex blo vl)
            levelMap' = HashMap.insert bli vl levelMap
            bli' = bli + 1
         in if
                | null tags ->
                    -- There are no constructors
                    compileDefault err levelMap' bli' blo vs' col matrix'
                | otherwise ->
                    -- Section 4, case 3(a)
                    let ind = fromJust (HashMap.lookup (List.head tags) (tab ^. infoConstructors)) ^. constructorInductive
                        ctrsNum = length (fromJust (HashMap.lookup ind (tab ^. infoInductives)) ^. inductiveConstructors)
                     in NCase
                          Case
                            { _caseInfo = mempty,
                              _caseInductive = ind,
                              _caseValue = val,
                              _caseBranches = map (compileBranch err levelMap' bli' blo vs' col matrix') tags,
                              _caseDefault =
                                if
                                    | length tags == ctrsNum ->
                                        Nothing
                                    | otherwise ->
                                        Just $ compileDefault err levelMap' bli' blo vs' col matrix'
                            }

    adjustIndex :: HashMap Level Level -> Level -> Index -> Index
    adjustIndex levelMap bl idx =
      case HashMap.lookup (getBinderLevel bl idx) levelMap of
        Nothing -> idx
        Just lvl -> getBinderIndex bl lvl

    updateLevelMap :: HashMap Level Level -> Level -> [Level] -> HashMap Level Level
    updateLevelMap levelMap bli vs =
      snd $
        foldl'
          ( \(bl, mp) vl ->
              (bl + 1, HashMap.insert bl vl mp)
          )
          (bli, levelMap)
          vs

    -- The `_patternRowBindersNum` field of the resulting matrix is not
    -- adjusted.
    decompose :: PatternMatrix -> ([Pattern], PatternMatrix)
    decompose matrix = (col, matrix')
      where
        col = map (List.head . (^. patternRowPatterns)) matrix
        matrix' = map (over patternRowPatterns List.tail) matrix

    getPatTags :: [Pattern] -> HashSet Tag
    getPatTags = \case
      [] ->
        mempty
      PatConstr PatternConstr {..} : pats ->
        HashSet.insert _patternConstrTag (getPatTags pats)
      _ : pats ->
        getPatTags pats

    compileMatchingRow :: HashMap Level Level -> Level -> Level -> [Level] -> PatternRow -> Node
    compileMatchingRow levelMap bli blo vs PatternRow {..} =
      assert (length _patternRowPatterns == length vs) $
        assert (bli == blo - length vs) $
          convert levelMap' blo _patternRowBody
      where
        levelMap' = updateLevelMap levelMap bli vs

    -- `compileBranch` computes S(c, M) where `c = Constr tag` and `M =
    -- col:matrix`, as described in Section 2, Figure 1 in the paper. Then it
    -- continues compilation with the new matrix.
    compileBranch :: ([Doc Ann] -> Doc Ann) -> HashMap Level Level -> Level -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Tag -> CaseBranch
    compileBranch err levelMap bli blo vs col matrix tag =
      CaseBranch
        { _caseBranchInfo = mempty,
          _caseBranchTag = tag,
          _caseBranchBinders = map mkBinder' argtys,
          _caseBranchBindersNum = argsNum,
          _caseBranchBody = compile err' levelMap' bli blo' (vs' ++ vs) matrix'
        }
      where
        argtys = typeArgs $ fromJust (HashMap.lookup tag (tab ^. infoConstructors)) ^. constructorType
        argsNum = length argtys
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
                                  _patternConstrArgs ++ row ^. patternRowPatterns
                              }
                    _ ->
                      Nothing
              )
              col
              matrix
        vs' = [blo .. blo + argsNum - 1]
        levelMap' = updateLevelMap levelMap bli vs'
        blo' = blo + argsNum
        err' args =
          err
            (parens (foldl' (<+>) (doc defaultOptions tag) (take argsNum args)) : drop argsNum args)

    -- `compileDefault` computes D(M) where `M = col:matrix`, as described in
    -- Section 2, Figure 1 in the paper. Then it continues compilation with the
    -- new matrix.
    compileDefault :: ([Doc Ann] -> Doc Ann) -> HashMap Level Level -> Level -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Node
    compileDefault err levelMap bli blo vs col matrix =
      compile err' levelMap bli blo vs matrix'
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
        err' args = err ("_" : args)
