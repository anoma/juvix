module Juvix.Compiler.Core.Transformation.MatchToCase where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pretty hiding (Options)
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
matchToCase :: Members '[Error CoreError, Reader Options] r => InfoTable -> Sem r InfoTable
matchToCase tab = runReader tab $ mapAllNodesM (rmapM goMatchToCase) tab

goMatchToCase ::
  forall r.
  Members '[Error CoreError, Reader Options, Reader InfoTable] r =>
  ([BinderChange] -> Node -> Sem r Node) ->
  Node ->
  Sem r Node
goMatchToCase recur node = case node of
  NMatch m ->
    compileMatch m
  _ ->
    recur [] node
  where
    compileMatch :: Match -> Sem r Node
    compileMatch Match {..} =
      go 0 (zipExact (toList _matchValues) (toList _matchValueTypes))
      where
        go :: Int -> [(Node, Type)] -> Sem r Node
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
          (val, valty) : vs' -> do
            ty' <- goMatchToCase (recur . (BCAdd n :)) valty
            val' <- goMatchToCase (recur . (BCAdd n :)) val
            mkLet' ty' val' <$> go (n + 1) vs'

    -- `compile err bindersNum vs matrix`:
    --  - `err` creates a textual representation of an unmatched pattern
    --    sequence, given as arguments the representations of patterns for the
    --    holes (corresponding to the matched values `vs`)
    --  - `bindersNum` is the number of binders added so far
    --  - `vs` are the de Bruijn levels of the values matched on w.r.t. the
    --    first added binder; a value matched on is always a variable referring
    --    to one of the binders added so far
    --  - `matrix` is the pattern matching matrix
    compile :: ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> PatternMatrix -> Sem r Node
    compile err bindersNum vs matrix = case matrix of
      [] -> do
        -- The matrix has no rows -- matching fails (Section 4, case 1).
        fCoverage <- asks (^. optCheckCoverage)
        if
            | fCoverage ->
                throw
                  CoreError
                    { _coreErrorMsg = "Pattern matching not exhaustive. Example pattern sequence not matched: " <> show (ppOutput $ err (repeat "_")),
                      _coreErrorNode = Nothing,
                      _coreErrorLoc = fromMaybe defaultLoc (getNodeLocation node)
                    }
            | otherwise ->
                return $
                  mkBuiltinApp' OpFail [mkConstant' (ConstString ("Pattern sequence not matched: " <> pat))]
        where
          pat = show (ppOutput $ err (repeat "_"))
          mockFile = $(mkAbsFile "/match-to-case")
          defaultLoc = singletonInterval (mkInitialLoc mockFile)
      r@PatternRow {..} : _
        | all isPatWildcard _patternRowPatterns ->
            -- The first row matches all values (Section 4, case 2)
            compileMatchingRow bindersNum vs r
      _ -> do
        -- Section 4, case 3
        -- Select the first column
        tab <- ask
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
                    compileDefault (missingTag tab ind tagsSet) err bindersNum vs' col matrix'
                | otherwise -> do
                    -- Section 4, case 3(a)
                    branches <- mapM (compileBranch err bindersNum vs' col matrix') tags
                    defaultBranch <-
                      if
                          | length tags == ctrsNum ->
                              return Nothing
                          | otherwise ->
                              Just <$> compileDefault (missingTag tab ind tagsSet) err bindersNum vs' col matrix'
                    return $
                      NCase
                        Case
                          { _caseInfo = mempty,
                            _caseInductive = ind,
                            _caseValue = val,
                            _caseBranches = branches,
                            _caseDefault = defaultBranch
                          }

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

    missingTag :: InfoTable -> Symbol -> HashSet Tag -> Tag
    missingTag tab ind tags = fromJust $ find (not . flip HashSet.member tags) (map (^. constructorTag) (ii ^. inductiveConstructors))
      where
        ii = fromJust $ HashMap.lookup ind (tab ^. infoInductives)

    compileMatchingRow :: Level -> [Level] -> PatternRow -> Sem r Node
    compileMatchingRow bindersNum vs PatternRow {..} =
      goMatchToCase (recur . (bcs ++)) _patternRowBody
      where
        bcs =
          reverse $
            foldl'
              ( \acc (pat, vl) ->
                  BCRemove (BinderRemove (getPatternBinder pat) (mkVal bindersNum vl)) : acc
              )
              _patternRowBinderChangesRev
              (drop _patternRowIgnoredPatternsNum (zipExact _patternRowPatterns vs))

    -- `compileDefault` computes D(M) where `M = col:matrix`, as described in
    -- Section 2, Figure 1 in the paper. Then it continues compilation with the
    -- new matrix.
    compileDefault :: Tag -> ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Sem r Node
    compileDefault tag err bindersNum vs col matrix = do
      tab <- ask
      let ci = fromJust $ HashMap.lookup tag (tab ^. infoConstructors)
          paramsNum = getTypeParamsNum tab (ci ^. constructorType)
          argsNum = ci ^. constructorArgsNum - paramsNum
          err' args = err (parensIf (argsNum > 0) (foldl' (<+>) (pretty (ci ^. constructorName)) (replicate argsNum "_")) : args)
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

    -- `compileBranch` computes S(c, M) where `c = Constr tag` and `M =
    -- col:matrix`, as described in Section 2, Figure 1 in the paper. Then it
    -- continues compilation with the new matrix.
    compileBranch :: ([Doc Ann] -> Doc Ann) -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Tag -> Sem r CaseBranch
    compileBranch err bindersNum vs col matrix tag = do
      tab <- ask
      let ci = fromJust $ HashMap.lookup tag (tab ^. infoConstructors)
          paramsNum = getTypeParamsNum tab (ci ^. constructorType)
          argsNum = length (typeArgs (ci ^. constructorType))
          bindersNum' = bindersNum + argsNum
          vs' = [bindersNum .. bindersNum + argsNum - 1]
          err' args =
            err
              (parensIf (argsNum > paramsNum) (foldl' (<+>) (pretty (ci ^. constructorName)) (drop paramsNum (take argsNum args))) : drop argsNum args)
      binders' <- getBranchBinders col matrix tag
      matrix' <- getBranchMatrix col matrix tag
      body <- compile err' bindersNum' (vs' ++ vs) matrix'
      return $
        CaseBranch
          { _caseBranchInfo = setInfoName (ci ^. constructorName) mempty,
            _caseBranchTag = tag,
            _caseBranchBinders = binders',
            _caseBranchBindersNum = argsNum,
            _caseBranchBody = body
          }

    getBranchBinders :: [Pattern] -> PatternMatrix -> Tag -> Sem r [Binder]
    getBranchBinders col matrix tag =
      reverse . snd
        <$> foldl'
          ( \a pat -> do
              (rbcs, acc) <- a
              let bcs = map (\b -> BCRemove (BinderRemove b (error "pattern compiler: dependently typed pattern"))) (getPatternExtraBinders pat)
                  bc = BCRemove (BinderRemove (getPatternBinder pat) (mkVar' 0))
              binder <- overM binderType (goMatchToCase (recur . revAppend rbcs)) (getPatternBinder pat)
              return (revAppend bcs (bc : BCAdd 1 : rbcs), binder : acc)
          )
          (return (matrix !! argPatsIndex ^. patternRowBinderChangesRev, []))
          argPats
      where
        argPats =
          List.head $
            mapMaybe
              ( \case
                  PatConstr PatternConstr {..}
                    | _patternConstrTag == tag ->
                        Just _patternConstrArgs
                  _ ->
                    Nothing
              )
              col
        argPatsIndex =
          fromJust $
            findIndex
              ( \case
                  PatConstr PatternConstr {..}
                    | _patternConstrTag == tag ->
                        True
                  _ ->
                    False
              )
              col

    getBranchMatrix :: [Pattern] -> PatternMatrix -> Tag -> Sem r PatternMatrix
    getBranchMatrix col matrix tag = do
      tab <- ask
      let ci = fromJust $ HashMap.lookup tag (tab ^. infoConstructors)
          argtys = typeArgs (ci ^. constructorType)
          argsNum = length argtys
      return $
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
