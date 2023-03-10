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
    -- | Current de Bruijn level in the input node
    _patternRowLevel :: Level,
    -- | The number of initial wildcard binders that don't increase the input de
    -- Bruijn level
    _patternRowIgnoredBindersNum :: Int,
    -- | Maps input de Bruijn levels to output de Bruijn levels
    _patternRowLevelMap :: HashMap Level Level
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
    go levelMap bl node0 = case node0 of
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
                compile err (bl + n) [bl .. bl + n - 1] matrix
              )
              (zipExact (toList _matchValues) (toList _matchValueTypes))
        where
          n = length _matchValues
          err = hsep . take n
          matrix = map matchBranchToPatternRow _matchBranches

          matchBranchToPatternRow :: MatchBranch -> PatternRow
          matchBranchToPatternRow MatchBranch {..} =
            PatternRow
              { _patternRowPatterns = toList _matchBranchPatterns,
                _patternRowBody = _matchBranchBody,
                _patternRowLevel = bl,
                _patternRowIgnoredBindersNum = 0,
                _patternRowLevelMap = levelMap
              }
      node ->
        Recur' (levelMap, node)

    -- `compile err levels bl vls matrix`:
    --  - `err` creates a textual representation of an unmatched pattern
    --    sequence, given as arguments the representations of patterns for the
    --    holes (corresponding to the matched values `vs`)
    --  - `blo` is the current de Bruijn level in the output node
    --  - `vs` are the de Bruijn levels of the values matched on, w.r.t. the
    --    output node
    --  - `matrix` is the pattern matching matrix
    compile :: ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> PatternMatrix -> Node
    compile err blo vs matrix = case matrix of
      [] ->
        -- The matrix has no rows -- matching fails (Section 4, case 1).
        error $ "Pattern matching not exhaustive. Example pattern sequence not matched: " <> show (ppOutput $ err (repeat "_"))
      r@PatternRow {..} : _
        | all isPatWildcard _patternRowPatterns ->
            -- The first row matches all values (Section 4, case 2)
            compileMatchingRow blo vs r
      _ ->
        -- Section 4, case 3
        -- Select the first column
        let vl = List.head vs
            vs' = List.tail vs
            val = mkVar' (getBinderIndex blo vl)
            (col, matrix') = decompose vl matrix
            tags = toList (getPatTags col)
         in if
                | null tags ->
                    -- There are no constructors
                    compileDefault err blo vs' col matrix'
                | otherwise ->
                    -- Section 4, case 3(a)
                    let ind = fromJust (HashMap.lookup (List.head tags) (tab ^. infoConstructors)) ^. constructorInductive
                        ctrsNum = length (fromJust (HashMap.lookup ind (tab ^. infoInductives)) ^. inductiveConstructors)
                     in NCase
                          Case
                            { _caseInfo = mempty,
                              _caseInductive = ind,
                              _caseValue = val,
                              _caseBranches = map (compileBranch err blo vs' col matrix') tags,
                              _caseDefault =
                                if
                                    | length tags == ctrsNum ->
                                        Nothing
                                    | otherwise ->
                                        Just $ compileDefault err blo vs' col matrix'
                            }

    adjustIndex :: HashMap Level Level -> Level -> Index -> Index
    adjustIndex levelMap bl idx =
      case HashMap.lookup (getBinderLevel bl idx) levelMap of
        Nothing -> idx
        Just lvl -> getBinderIndex bl lvl

    decompose :: Level -> PatternMatrix -> ([Pattern], PatternMatrix)
    decompose vl matrix = (col, matrix')
      where
        col = map (List.head . (^. patternRowPatterns)) matrix
        matrix' = map updateRow matrix

        updateRow :: PatternRow -> PatternRow
        updateRow row =
          row
            { _patternRowPatterns = List.tail (row ^. patternRowPatterns),
              _patternRowLevel =
                if
                    | nIgnored > 0 -> bli
                    | otherwise -> bli + 1,
              _patternRowIgnoredBindersNum = max 0 (nIgnored - 1),
              _patternRowLevelMap =
                if
                    | nIgnored > 0 -> levelMap
                    | otherwise -> HashMap.insert bli vl levelMap
            }
          where
            bli = row ^. patternRowLevel
            nIgnored = row ^. patternRowIgnoredBindersNum
            levelMap = row ^. patternRowLevelMap

    getPatTags :: [Pattern] -> HashSet Tag
    getPatTags = \case
      [] ->
        mempty
      PatConstr PatternConstr {..} : pats ->
        HashSet.insert _patternConstrTag (getPatTags pats)
      _ : pats ->
        getPatTags pats

    compileMatchingRow :: Level -> [Level] -> PatternRow -> Node
    compileMatchingRow blo vs PatternRow {..} =
      assert (length _patternRowPatterns == length vs) $
        convert levelMap' blo _patternRowBody
      where
        levelMap' =
          snd $
            foldl'
              ( \(bl, mp) vl ->
                  (bl + 1, HashMap.insert bl vl mp)
              )
              (_patternRowLevel, _patternRowLevelMap)
              (drop _patternRowIgnoredBindersNum vs)

    -- `compileBranch` computes S(c, M) where `c = Constr tag` and `M =
    -- col:matrix`, as described in Section 2, Figure 1 in the paper. Then it
    -- continues compilation with the new matrix.
    compileBranch :: ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Tag -> CaseBranch
    compileBranch err blo vs col matrix tag =
      CaseBranch
        { _caseBranchInfo = setInfoName (ci ^. constructorName) mempty,
          _caseBranchTag = tag,
          _caseBranchBinders = map mkBinder' argtys,
          _caseBranchBindersNum = argsNum,
          _caseBranchBody = compile err' blo' (vs' ++ vs) matrix'
        }
      where
        ci = fromJust $ HashMap.lookup tag (tab ^. infoConstructors)
        argtys = typeArgs $ fromJust (HashMap.lookup tag (tab ^. infoConstructors)) ^. constructorType
        argsNum = length argtys
        blo' = blo + argsNum
        vs' = [blo .. blo + argsNum - 1]
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
                    PatWildcard {} ->
                      Just $
                        row
                          { _patternRowPatterns =
                              map (PatWildcard . PatternWildcard mempty . Binder "_" Nothing) argtys
                                ++ row ^. patternRowPatterns,
                            _patternRowIgnoredBindersNum = row ^. patternRowIgnoredBindersNum + argsNum
                          }
                    _ ->
                      Nothing
              )
              col
              matrix
        err' args =
          err
            (parens (foldl' (<+>) (pretty (ci ^. constructorName)) (take argsNum args)) : drop argsNum args)

    -- `compileDefault` computes D(M) where `M = col:matrix`, as described in
    -- Section 2, Figure 1 in the paper. Then it continues compilation with the
    -- new matrix.
    compileDefault :: ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Node
    compileDefault err blo vs col matrix =
      compile err' blo vs matrix'
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
