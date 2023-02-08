module Juvix.Compiler.Core.Transformation.MatchToCase where

import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)
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
        numValues = length values

    -- Index from 1 because we prepend the fail branch.
    branchNodes <-
      (failNode numValues :)
        <$> mapM compileMatchBranch (indexFrom 1 (reverse branches))

    let appNode = mkApps' (mkVar' 0) (shift (length branchNodes) <$> (reverse values))
    return (foldr mkLet' appNode branchNodes)
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
--              cases).
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
--      λ? λ? match ?$0, ?$1 with {
--        xs, x ↦ match xs$1 with {
--            :: _ y (:: _ z ys) ↦ + (+ x$3 y$2) z$1
--        }
--      }
-- @
--
-- The body of the match branch is @ + (+ x$3 y$2) z$1 @, the @x@ variable is
-- free in the inner match, so it needs to be shifted so it continues to point
-- to the @x@ bound in the outer match after additional binders have been added.
--
-- The inner match compiles to:
--
-- @
--    λ? let ? := λ? fail "Non-exhaustive patterns" in
--       let ? := λ? case ?$0 of {
--              :: _ y arg_27 := case ?$0 of {
--                 :: _ z ys := let y := ?$4 in
--                              let z := ?$2 in
--                              let ys := ?$2 in
--                                 + (+ x$11 y$2) z$1;
--                 _ := ?$4 ?$3
--              };
--              _ := ?$1 ?$0
--            } in
--        ?$0 ?$2
-- @
--
-- The body is wrapped in let bindings for `y`, `z` and `ys` in the
-- correct order so that the indices of `y` and `z` in the body point to the
-- correct variables above.
--
-- The index for the free variable @x@ in the body has increased from 3 to 11.
-- This is because we have added 3 binders around the body, 3 auxillary binders
-- (arg_27 and two wildcards), 1 binder for the lambda surrounding the case and
-- 1 binder for the fail branch.
compileMatchBranch :: forall r. Members '[InfoTableBuilder] r => Indexed MatchBranch -> Sem r Node
compileMatchBranch (Indexed branchNum br) = do
  compiledBranch <- runReader initState (combineCompiledPatterns (map (compilePattern patternsNum) patterns))
  return (mkLambdas' patternsNum ((compiledBranch ^. compiledPatMkNode) (wrapBody (compiledBranch ^. compiledPatBinders))))
  where
    patterns :: [Pattern]
    patterns = toList (br ^. matchBranchPatterns)

    patternsNum = length patterns

    wrapBody :: [CompiledBinder] -> Node
    wrapBody binders = foldr (uncurry (mkLet mempty)) shiftedBody vars
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

combineCompiledPatterns :: forall r. Member (Reader CompileState) r => [Sem r CompiledPattern] -> Sem r CompiledPattern
combineCompiledPatterns = go . indexFrom 1
  where
    go :: [Indexed (Sem r CompiledPattern)] -> Sem r CompiledPattern
    go [] = asks (^. compileStateCompiledPattern)
    go (Indexed depth cp : xs) = do
      newBinders <- (^. compiledPatBinders) <$> cp
      newMkBody <- (^. compiledPatMkNode) <$> cp
      prevBinders <- asks (^. compileStateCompiledPattern . compiledPatBinders)
      prevMkBody <- asks (^. compileStateCompiledPattern . compiledPatMkNode)
      let updatedBinders = prevBinders ++ newBinders
      local
        ( (over compileStateBindersAbove (+ length updatedBinders))
            . (set (compileStateCompiledPattern . compiledPatBinders) updatedBinders)
            . (set (compileStateCompiledPattern . compiledPatMkNode) (prevMkBody . newMkBody))
            . (set compileStateCurrentNode (mkVar' (length updatedBinders + depth)))
        )
        (go xs)

compilePattern :: forall r. Members [Reader CompileState, InfoTableBuilder] r => Int -> Pattern -> Sem r CompiledPattern
compilePattern numPatterns = \case
  PatWildcard {} -> return (CompiledPattern [] id)
  PatBinder b -> do
    subPats <- resetCurrentNode (incBindersAbove (compilePattern numPatterns (b ^. patternBinderPattern)))
    currentNode <- asks (^. compileStateCurrentNode)
    let newBinder = b ^. patternBinder
    return
      CompiledPattern
        { _compiledPatBinders = OriginalBinder newBinder : (subPats ^. compiledPatBinders),
          _compiledPatMkNode = (mkLet mempty newBinder currentNode) . (subPats ^. compiledPatMkNode)
        }
  PatConstr c -> do
    let args = (c ^. patternConstrArgs)
    argBinders <- mapM mkCompiledBinder args
    compiledArgs <- mkArgCompiledPattern args
    let vs = compiledArgs ^. compiledPatBinders
        nn = compiledArgs ^. compiledPatMkNode
    binders <- mapM mkBinder args
    newMkNode <- mkCaseFromBinders binders

    return
      ( CompiledPattern
          { _compiledPatBinders = argBinders ++ vs,
            _compiledPatMkNode = newMkNode . nn
          }
      )
    where
      mkArgCompiledPattern :: [Pattern] -> Sem r CompiledPattern
      mkArgCompiledPattern args =
        let ctorArgsPatterns = compilePattern numPatterns <$> [cp | cp@(PatConstr {}) <- reverse args]
         in runReader (stateWithBindersAbove (length args)) (combineCompiledPatterns ctorArgsPatterns)

      mkCompiledBinder :: Pattern -> Sem r CompiledBinder
      mkCompiledBinder p = case p of
        PatBinder {} -> OriginalBinder <$> binder
        PatWildcard {} -> AuxiliaryBinder <$> binder
        PatConstr {} -> AuxiliaryBinder <$> binder
        where
          binder :: Sem r Binder
          binder = mkBinder p

      mkBinder :: Pattern -> Sem r Binder
      mkBinder = \case
        PatBinder b -> return (b ^. patternBinder)
        PatWildcard {} ->
          return
            Binder
              { _binderName = "_",
                _binderLocation = Nothing,
                _binderType = mkDynamic'
              }
        PatConstr {} -> do
          argSym <- freshSymbol
          return
            Binder
              { _binderName = uniqueName "arg" argSym,
                _binderLocation = Nothing,
                _binderType = mkDynamic'
              }

      mkCaseFromBinders :: [Binder] -> Sem r (Node -> Node)
      mkCaseFromBinders binders = do
        indSym <- (^. constructorInductive) <$> ctorInfo
        currentNode <- asks (^. compileStateCurrentNode)
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

failNode :: Int -> Node
failNode n = mkLambdas' n (mkBuiltinApp' OpFail [mkConstant' (ConstString "Non-exhaustive patterns")])

defaultNode' :: Member (Reader CompileState) r => Int -> Sem r Node
defaultNode' numMatchValues = do
  numBindersAbove <- asks (^. compileStateBindersAbove)
  return
    ( mkApps'
        (mkVar' (numBindersAbove + numMatchValues))
        (mkVar' <$> (reverse (take numMatchValues [numBindersAbove ..])))
    )
