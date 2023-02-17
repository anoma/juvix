module Juvix.Compiler.Core.Transformation.MatchToCase where

import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)
import Juvix.Compiler.Core.Info.TypeInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.MatchToCase.Data

matchToCase :: InfoTable -> InfoTable
matchToCase = run . mapT' (const (umapM matchToCaseNode))

matchToCaseNode :: forall r. Member InfoTableBuilder r => Node -> Sem r Node
matchToCaseNode n = case n of
  NMatch m -> do
    let branches = m ^. matchBranches
        values = toList (m ^. matchValues)
        matchType = getInfoType (m ^. matchInfo)
        -- TODO: `getInfoType` always returns Dynamic at this stage, because the
        -- Internal-to-Core translation doesn't store types in the Infos
        valueTypes = (getInfoType . getInfo <$> values)
        branchType = mkPis' valueTypes matchType

    -- Index from 1 because we prepend the fail branch.
    branchNodes <-
      (failNode valueTypes :)
        <$> mapM compileMatchBranch (indexFrom 1 (reverse branches))

    -- The appNode calls the first branch with the values of the match
    let appNode = mkApps' (mkVar' 0) (shift (length branchNodes) <$> values)
    return (foldr (mkLet' branchType) appNode branchNodes)
  _ -> return n

-- | increase all free variable indices by a given value.
-- In this function we consider indices to be embedded at a specified level
shiftEmbedded :: Level -> Index -> Node -> Node
shiftEmbedded _ 0 = id
shiftEmbedded wrappingLevel m = umapN go
  where
    go k = \case
      NVar v
        | v ^. varIndex >= (k + wrappingLevel) -> NVar (shiftVar m v)
      n -> n

-- | Returns a modified MatchBranch body where:
--
--        1. The body is wrapped in let bindings so that bound variables in the
--        body point to the correct variables.
--
--        2. Free variables in the body are shifted by the sum of:
--
--            * The number of let bindings added in step 1, equal to the total
--              number of pattern binders in the matchbranch.
--
--            * The auxillary bindings added in the translation (i.e bindings
--              not present in the original match bindings, added for nested
--              cases and case bindings).
--
--            * The number of previously bound matchbranches including the fail branch.
--
--            * The total number of match patterns (because each match pattern
--              is translated to a lambda argument surrounding the compiled
--              branch).
--
-- For example:
--
--  @
--      f : Nat -> List Nat -> Nat;
--      f x xs := case xs
--         | (y :: z :: ys) := x + y + z;
--  @
--
-- Translates to the following nested matches:
--
-- @
--       λ? λ? match ?$1, ?$0 with {
--          x, xs ↦ match xs$0 with {
--            :: _ y (:: _ z ys) ↦ + (+ x$4 y$2) z$1
--          }
-- @
--
-- The body of the match branch is @ + (+ x$3 y$2) z$1 @, the @x@ variable is
-- free in the inner match, so it needs to be shifted so it continues to point
-- to the @x@ bound in the outer match after additional binders have been added.
--
-- The inner match compiles to:
--
-- @
--    λ?
--         let ? := λ? fail "Non-exhaustive patterns" in
--         let ? := λ? case ?$0 of {
--           :: _ y arg_11 := let y := ?$1 in case ?$1 of {
--             :: _ z ys := let z := ?$1 in
--                          let ys := ?$1 in
--                              let y := ?$5 in
--                              let z := ?$2 in
--                              let ys := ?$2 in + (+ x$15 y$2) z$1;
--             _ := ?$5 ?$4
--           };
--           _ := ?$1 ?$0
-- @
--
-- The body is wrapped in let bindings for `y`, `z` and `ys` in the
-- correct order so that the indices of `y` and `z` in the body point to the
-- correct variables above.
--
-- The index for the free variable @x@ in the body has increased from 4 to 15.
-- This is because we have added 3 binders around the body, 6 auxillary binders,
-- 1 binder for the lambda surrounding the case and 1 binder for the fail
-- branch.
compileMatchBranch :: forall r. Members '[InfoTableBuilder] r => Indexed MatchBranch -> Sem r Node
compileMatchBranch (Indexed branchNum br) = do
  compiledBranch <- runReader initState' (combineCompiledPatterns (map (compilePattern patternsNum) patterns))
  return (mkLambdas' (patternType <$> patterns) ((compiledBranch ^. compiledPatMkNode) (wrapBody (compiledBranch ^. compiledPatBinders))))
  where
    patterns :: [Pattern]
    patterns = toList (br ^. matchBranchPatterns)

    patternsNum = length patterns

    wrapBody :: [CompiledBinder] -> Node
    wrapBody binders = foldr (uncurry (mkLet mempty . set binderType mkDynamic')) shiftedBody vars
      where
        vars :: [(Binder, Node)]
        vars = second mkVar' . swap . toTuple <$> extractOriginalBinders binders

        shiftedBody :: Node
        shiftedBody =
          let patternBindersNum' = length (concatMap getPatternBinders patterns)
              auxiliaryBindersNum = length (filter isAuxiliaryBinder binders)
           in shiftEmbedded
                patternBindersNum'
                (auxiliaryBindersNum + patternBindersNum' + patternsNum + branchNum)
                (br ^. matchBranchBody)

    initState' :: CompileState
    initState' =
      CompileState
        { _compileStateBindersAbove = 0,
          _compileStateCompiledPattern = mempty
        }

-- | Extract original binders (i.e binders which are referenced in the match
-- branch body) from a list of `CompiledBinder`s indexed by the total number
-- (i.e including the auxiliary binders) of binders below it.
-- The `CompiledBinders` should be passed to this function in the order that they
-- were introduced.
extractOriginalBinders :: [CompiledBinder] -> [Indexed Binder]
extractOriginalBinders vs = updateBinders $ fmap getBinder <$> reverse (filterIndexed isOriginalBinder (indexFrom 0 (reverse vs)))
  where
    updateBinders :: [Indexed a] -> [Indexed a]
    updateBinders = zipWith (over indexedIx . (+)) [0 ..]

-- | Combine the results of compiling the patterns of a match branch or patterns of constructor arguments.
--
-- If the arguments are a_1, .... a_n then the first pattern refers to its argument by index (n - 1), the second argument
-- refers to its argument by index (n - 2) and so on. This is the purpose of the indexedPatterns and setting the CompileStateNode.
--
-- The patterns are then evaluated and combined from left to right in the list .
combineCompiledPatterns :: forall r. Member (Reader CompileState) r => [Sem ((Reader CompileStateNode) ': r) CompiledPattern] -> Sem r CompiledPattern
combineCompiledPatterns ps = go indexedPatterns
  where
    indexedPatterns :: [Indexed (Sem ((Reader CompileStateNode) ': r) CompiledPattern)]
    indexedPatterns = reverse (indexFrom 0 (reverse ps))

    go :: [Indexed (Sem ((Reader CompileStateNode) ': r) CompiledPattern)] -> Sem r CompiledPattern
    go [] = asks (^. compileStateCompiledPattern)
    go (Indexed depth cp : xs) = do
      numBinders <- length <$> asks (^. compileStateCompiledPattern . compiledPatBinders)
      nextPattern <- runReader (CompileStateNode (mkVar' (numBinders + depth))) cp
      updateState nextPattern (go xs)
      where
        updateState :: CompiledPattern -> Sem r CompiledPattern -> Sem r CompiledPattern
        updateState p =
          local
            ( over compileStateBindersAbove (+ length (p ^. compiledPatBinders))
                . (over compileStateCompiledPattern (<> p))
            )

-- | Compile a single pattern
--
-- A Wildcard introduces no new binders and do not modify the body.
--
-- A Binder introduces a binder and may also name a subpattern (i.e an as-pattern)
--
-- A Constructor is translated into a case statement. Each of its arguments
-- (wildcard, binder or constructor) introduces an auxiliary binder.
-- The arguments are then compiled recursively using a new CompileState context.
-- The default case points to the next branch pattern.
compilePattern :: forall r. Members [Reader CompileState, Reader CompileStateNode, InfoTableBuilder] r => Int -> Pattern -> Sem r CompiledPattern
compilePattern numPatterns = \case
  PatWildcard {} -> return (CompiledPattern [] id)
  PatBinder b -> do
    subPats <- resetCurrentNode (incBindersAbove (compilePattern numPatterns (b ^. patternBinderPattern)))
    currentNode <- asks (^. compileStateNodeCurrent)
    let newBinder = set binderType mkDynamic' $ b ^. patternBinder -- TODO: this is a hack to avoid problems with de Bruijn
    let compiledBinder =
          CompiledPattern
            { _compiledPatBinders = [OriginalBinder newBinder],
              _compiledPatMkNode = mkLet mempty newBinder currentNode
            }
    return (compiledBinder <> subPats)
  PatConstr c -> do
    let args = (c ^. patternConstrArgs)
    compiledArgs <- compileArgs args
    compiledCase <- compileCase args
    return (compiledCase <> compiledArgs)
    where
      compileCase :: [Pattern] -> Sem r CompiledPattern
      compileCase args = do
        binders <- mapM mkBinder'' args
        CompiledPattern <$> mapM mkCompiledBinder args <*> mkCaseFromBinders binders

      compileArgs :: [Pattern] -> Sem r CompiledPattern
      compileArgs args = do
        bindersAbove <- asks (^. compileStateBindersAbove)
        let ctorArgsPatterns = compilePattern numPatterns <$> args
            state = mkState bindersAbove
        runReader state (combineCompiledPatterns ctorArgsPatterns)
        where
          mkState :: Int -> CompileState
          mkState bindersAbove =
            ( CompileState
                { _compileStateBindersAbove = bindersAbove + length args,
                  _compileStateCompiledPattern = mempty
                }
            )

      mkCompiledBinder :: Pattern -> Sem r CompiledBinder
      mkCompiledBinder p = AuxiliaryBinder <$> mkBinder'' p

      mkBinder'' :: Pattern -> Sem r Binder
      mkBinder'' = \case
        PatBinder b -> return (b ^. patternBinder)
        PatWildcard w -> do
          let info = w ^. patternWildcardInfo
          return
            Binder
              { _binderName = "_",
                _binderLocation = getInfoLocation info,
                _binderType = getInfoType info
                -- TODO: `getInfoType` always returns Dynamic at this stage
              }
        PatConstr c' -> do
          let info = c' ^. patternConstrInfo
          -- TODO: `getInfoType` always returns Dynamic at this stage
          mkUniqueBinder "arg" (getInfoLocation info) (getInfoType info)

      mkCaseFromBinders :: [Binder] -> Sem r (Node -> Node)
      mkCaseFromBinders binders = do
        indSym <- (^. constructorInductive) <$> ctorInfo
        currentNode <- asks (^. compileStateNodeCurrent)
        defaultNode'' <- defaultNode' numPatterns
        let mkCaseFromBranch :: CaseBranch -> Node
            mkCaseFromBranch b =
              mkCase
                mempty
                indSym
                currentNode
                [b]
                (Just defaultNode'')
        (mkCaseFromBranch .) <$> mkBranch
        where
          ctorInfo :: Sem r ConstructorInfo
          ctorInfo = getConstructorInfo (c ^. patternConstrTag)

          mkBranch :: Sem r (Node -> CaseBranch)
          mkBranch = do
            ctorName <- (^. constructorName) <$> ctorInfo
            return
              ( \next ->
                  CaseBranch
                    { _caseBranchInfo = setInfoName ctorName mempty,
                      _caseBranchTag = c ^. patternConstrTag,
                      _caseBranchBinders = binders,
                      _caseBranchBindersNum = length binders,
                      _caseBranchBody = next
                    }
              )

failNode :: [Type] -> Node
failNode tys = mkLambdas' tys (mkBuiltinApp' OpFail [mkConstant' (ConstString "Non-exhaustive patterns")])

mkUniqueBinder' :: Member InfoTableBuilder r => Text -> Node -> Sem r Binder
mkUniqueBinder' name ty = mkUniqueBinder name Nothing ty

mkUniqueBinder :: Member InfoTableBuilder r => Text -> Maybe Location -> Node -> Sem r Binder
mkUniqueBinder name loc ty = do
  sym <- freshSymbol
  return
    Binder
      { _binderName = uniqueName name sym,
        _binderLocation = loc,
        _binderType = ty
      }

-- | The default node in a case expression.
-- It points to the next branch above.
defaultNode' :: Member (Reader CompileState) r => Int -> Sem r Node
defaultNode' numMatchValues = do
  numBindersAbove <- asks (^. compileStateBindersAbove)
  return
    ( mkApps'
        (mkVar' (numBindersAbove + numMatchValues))
        (mkVar' <$> (reverse (take numMatchValues [numBindersAbove ..])))
    )
