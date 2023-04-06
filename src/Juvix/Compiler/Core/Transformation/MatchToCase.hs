module Juvix.Compiler.Core.Transformation.MatchToCase where

import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)
import Juvix.Compiler.Core.Language.Value
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
matchToCase :: Members '[Error CoreError, Reader CoreOptions] r => InfoTable -> Sem r InfoTable
matchToCase tab = runReader tab $ mapAllNodesM (rmapM goMatchToCase) tab

goMatchToCase ::
  forall r.
  Members '[Error CoreError, Reader CoreOptions, Reader InfoTable] r =>
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
              err = take n
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
    compile :: ([Value] -> [Value]) -> Level -> [Level] -> PatternMatrix -> Sem r Node
    compile err bindersNum vs matrix = case matrix of
      [] -> do
        -- The matrix has no rows -- matching fails (Section 4, case 1).
        fCoverage <- asks (^. optCheckCoverage)
        if
            | fCoverage ->
                throw
                  CoreError
                    { _coreErrorMsg =
                        ppOutput
                          ( "Pattern matching not exhaustive. Example pattern "
                              <> seq
                              <> "not matched: "
                              <> pat'
                          ),
                      _coreErrorNode = Nothing,
                      _coreErrorLoc = fromMaybe defaultLoc (getNodeLocation node)
                    }
            | otherwise ->
                return $
                  mkBuiltinApp' OpFail [mkConstant' (ConstString ("Pattern sequence not matched: " <> ppTrace pat))]
        where
          pat = err (replicate (length vs) ValueWildcard)
          seq = if length pat == 1 then "" else "sequence "
          pat' = if length pat == 1 then doc defaultOptions (List.head pat) else docValueSequence pat
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
         in if
                | null tags ->
                    -- There are no constructor patterns
                    compileDefault Nothing err bindersNum vs' col matrix'
                | otherwise -> do
                    -- Section 4, case 3(a)
                    let ind = lookupConstructorInfo tab (List.head tags) ^. constructorInductive
                        ctrsNum = length (lookupInductiveInfo tab ind ^. inductiveConstructors)
                    branches <- mapM (compileBranch err bindersNum vs' col matrix') tags
                    defaultBranch <-
                      if
                          | length tags == ctrsNum ->
                              return Nothing
                          | otherwise ->
                              Just <$> compileDefault (Just $ missingTag tab ind tagsSet) err bindersNum vs' col matrix'
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
                    | otherwise -> mkBCRemove binder val : rbcs
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
    missingTag tab ind tags = fromJust $ find (not . flip HashSet.member tags) (ii ^. inductiveConstructors)
      where
        ii = lookupInductiveInfo tab ind

    compileMatchingRow :: Level -> [Level] -> PatternRow -> Sem r Node
    compileMatchingRow bindersNum vs PatternRow {..} =
      goMatchToCase (recur . (bcs ++)) _patternRowBody
      where
        bcs =
          reverse $
            foldl'
              ( \acc (pat, vl) ->
                  mkBCRemove (getPatternBinder pat) (mkVal bindersNum vl) : acc
              )
              _patternRowBinderChangesRev
              (drop _patternRowIgnoredPatternsNum (zipExact _patternRowPatterns vs))

    -- `compileDefault` computes D(M) where `M = col:matrix`, as described in
    -- Section 2, Figure 1 in the paper. Then it continues compilation with the
    -- new matrix.
    compileDefault :: Maybe Tag -> ([Value] -> [Value]) -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Sem r Node
    compileDefault mtag err bindersNum vs col matrix = do
      tab <- ask
      compile (err' tab) bindersNum vs matrix'
      where
        matrix' = [row | (pat, row) <- zipExact col matrix, PatWildcard {} <- [pat]]
        err' tab args =
          case mtag of
            Just tag ->
              err (ctr : args)
              where
                ci = lookupConstructorInfo tab tag
                ii = lookupInductiveInfo tab (ci ^. constructorInductive)
                paramsNum = length (ii ^. inductiveParams)
                argsNum = ci ^. constructorArgsNum - paramsNum
                ctr =
                  ValueConstrApp
                    ConstrApp
                      { _constrAppName = ci ^. constructorName,
                        _constrAppFixity = ci ^. constructorFixity,
                        _constrAppArgs = replicate argsNum ValueWildcard
                      }
            Nothing ->
              err (ValueWildcard : args)

    -- `compileBranch` computes S(c, M) where `c = Constr tag` and `M =
    -- col:matrix`, as described in Section 2, Figure 1 in the paper. Then it
    -- continues compilation with the new matrix.
    compileBranch :: ([Value] -> [Value]) -> Level -> [Level] -> [Pattern] -> PatternMatrix -> Tag -> Sem r CaseBranch
    compileBranch err bindersNum vs col matrix tag = do
      tab <- ask
      let ci = lookupConstructorInfo tab tag
          ii = lookupInductiveInfo tab (ci ^. constructorInductive)
          paramsNum = length (ii ^. inductiveParams)
          argsNum = length (typeArgs (ci ^. constructorType))
          bindersNum' = bindersNum + argsNum
          vs' = [bindersNum .. bindersNum + argsNum - 1]
          err' args =
            err (ctr : drop argsNum args)
            where
              ctr =
                ValueConstrApp
                  ConstrApp
                    { _constrAppName = ci ^. constructorName,
                      _constrAppFixity = ci ^. constructorFixity,
                      _constrAppArgs = drop paramsNum (take argsNum args)
                    }
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
              let bcs = map (\b -> mkBCRemove b (error "pattern compiler: dependently typed pattern")) (getPatternExtraBinders pat)
                  bc = mkBCRemove (getPatternBinder pat) (mkVar' 0)
              binder <- overM binderType (goMatchToCase (recur . revAppend rbcs)) (getPatternBinder pat)
              return (revAppend bcs (bc : BCAdd 1 : rbcs), binder : acc)
          )
          (return (matrix !! (argPatsIx ^. indexedIx) ^. patternRowBinderChangesRev, []))
          (argPatsIx ^. indexedThing)
      where
        argPatsIx :: Indexed [Pattern]
        argPatsIx = fromJust (firstJust (mapM getArgs) (indexFrom 0 col))
          where
            getArgs :: Pattern -> Maybe [Pattern]
            getArgs = \case
              PatConstr PatternConstr {..}
                | _patternConstrTag == tag -> Just _patternConstrArgs
              _ -> Nothing

    getBranchMatrix :: [Pattern] -> PatternMatrix -> Tag -> Sem r PatternMatrix
    getBranchMatrix col matrix tag = do
      tab <- ask
      let ci = lookupConstructorInfo tab tag
          argtys = typeArgs (ci ^. constructorType)
          argsNum = length argtys
          helper :: PatternRow -> Pattern -> Maybe PatternRow
          helper row = \case
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
      return (catMaybes [helper row pat | (pat, row) <- zipExact col matrix])
