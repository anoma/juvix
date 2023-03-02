module Juvix.Compiler.Core.Transformation.MatchToCase where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

data PatternRow = PatternRow
  { _patternRowPatterns :: [Pattern],
    -- | The number of binders in `_patternRowPatterns`
    _patternRowBindersNum :: Int,
    _patternRowBody :: Node
  }

makeLenses ''PatternRow

type PatternMatrix = [PatternRow]

-- | Compiles pattern matches (`Match` nodes) to decision trees built up from
-- `Case` nodes. The algorithm is based on the paper: Luc Maranget, "Compiling
-- Pattern Matching to Good Decision Trees", ML'08.
matchToCase :: InfoTable -> InfoTable
matchToCase tab = mapAllNodes (convert [] 0) tab
  where
    -- See the comments above `compile` for the explanation of `levels` and
    -- `bl`.
    convert :: [Level] -> Level -> Node -> Node
    convert levels bl node = dmapCNR' (bl, go) levels node

    go :: [Level] -> Level -> Node -> Recur' [Level]
    go levels bl = \case
      NVar (Var {..}) ->
        End' (mkVar _varInfo (adjustIndex levels bl _varIndex))
      NMatch (Match {..}) ->
        End' $
          snd $
            foldr
              ( \(val, valty) (k, node) ->
                  let levels' = replicate k bl ++ levels
                   in ( k - 1,
                        mkLet'
                          (convert levels' bl valty)
                          (convert levels' bl val)
                          node
                      )
              )
              ( n - 1,
                compile err (replicate n bl ++ levels) bl [0 .. n - 1] matrix
              )
              (zipExact (toList _matchValues) (toList _matchValueTypes))
        where
          n = length _matchValues
          err = hsep . take n
          matrix = map matchBranchToPatternRow _matchBranches
      node ->
        Recur' (levels, node)

    matchBranchToPatternRow :: MatchBranch -> PatternRow
    matchBranchToPatternRow MatchBranch {..} =
      PatternRow
        { _patternRowPatterns = toList _matchBranchPatterns,
          _patternRowBindersNum = length (concatMap getPatternBinders _matchBranchPatterns),
          _patternRowBody = _matchBranchBody
        }

    -- `compile err levels bl vls matrix`:
    --  - `err` creates a textual representation of an unmatched pattern
    --    sequence, given as arguments the representations of patterns for the
    --    holes (corresponding to the matched values `vls`)
    --  - `levels` is the list of de Bruijn levels (w.r.t. the input node) at
    --    which binders are inserted in the output node
    --  - `bl` is the current de Bruijn level
    --  - `vs` are the de Bruijn indices of the values matched on, w.r.t. the
    --    output node
    --  - `matrix` is the pattern matching matrix
    compile :: ([Doc Ann] -> Doc Ann) -> [Level] -> Level -> [Index] -> PatternMatrix -> Node
    compile err levels bl vs matrix = case matrix of
      [] ->
        error $ show $ ppOutput $ err (repeat "_")
      r@PatternRow {..} : _
        | not (any isPatConstr _patternRowPatterns) ->
            -- The first row matches all values
            compileMatchingRow levels bl vs r
      _ ->
        let (col, matrix') = decompose matrix
            val = mkVar' (List.head vs)
            tags = toList (getPatTags col)
         in if
                | null tags ->
                    compileDefault err levels bl vs col matrix'
                | otherwise ->
                    let ind = fromJust (HashMap.lookup (List.head tags) (tab ^. infoConstructors)) ^. constructorInductive
                        ctrsNum = length (fromJust (HashMap.lookup ind (tab ^. infoInductives)) ^. inductiveConstructors)
                     in NCase
                          Case
                            { _caseInfo = mempty,
                              _caseInductive = ind,
                              _caseValue = val,
                              _caseBranches = map (compileBranch err levels bl vs col matrix') tags,
                              _caseDefault =
                                if
                                    | length tags == ctrsNum ->
                                        Nothing
                                    | otherwise ->
                                        Just $ compileDefault err levels bl vs col matrix'
                            }

    adjustIndex :: [Level] -> Level -> Index -> Index
    adjustIndex levels bl idx =
      idx + length (filter (\x -> x >= bl - idx) levels)

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

    compileMatchingRow :: [Level] -> Level -> [Index] -> PatternRow -> Node
    compileMatchingRow levels bl vs PatternRow {..} =
      snd $
        foldr
          ( \(pat, vi) (n, node) -> case pat of
              PatBinder PatternBinder {..} ->
                ( n - 1,
                  mkLet'
                    (convert levels (bl + n) (_patternBinder ^. binderType))
                    (mkVar' (vi + n))
                    node
                )
              _ ->
                (n, node)
          )
          (_patternRowBindersNum - 1, body)
          (reverse (zipExact _patternRowPatterns vs))
      where
        body = convert levels (bl + _patternRowBindersNum) _patternRowBody

    compileBranch :: ([Doc Ann] -> Doc Ann) -> [Level] -> Level -> [Index] -> [Pattern] -> PatternMatrix -> Tag -> CaseBranch
    compileBranch err levels bl vs col matrix tag =
      CaseBranch
        { _caseBranchInfo = mempty,
          _caseBranchTag = tag,
          _caseBranchBinders = map mkBinder' argtys,
          _caseBranchBindersNum = argsNum,
          _caseBranchBody = compile err' levels' bl vs' matrix'
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
        levels' = replicate argsNum bl ++ levels
        vs' = [0 .. argsNum - 1] ++ map (argsNum +) (List.tail vs)
        err' args =
          err
            (parens (foldl' (<+>) (doc defaultOptions tag) (take argsNum args)) : drop argsNum args)

    compileDefault :: ([Doc Ann] -> Doc Ann) -> [Level] -> Level -> [Index] -> [Pattern] -> PatternMatrix -> Node
    compileDefault = undefined
