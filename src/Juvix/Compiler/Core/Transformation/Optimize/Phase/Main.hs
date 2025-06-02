module Juvix.Compiler.Core.Transformation.Optimize.Phase.Main where

import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Extra.Dump
import Juvix.Compiler.Core.Extra.Utils (getTableSymbolsMap)
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.CaseFolding
import Juvix.Compiler.Core.Transformation.Optimize.CasePermutation
import Juvix.Compiler.Core.Transformation.Optimize.ConstantFolding
import Juvix.Compiler.Core.Transformation.Optimize.FilterUnreachable
import Juvix.Compiler.Core.Transformation.Optimize.Inlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.SimplifyArithmetic
import Juvix.Compiler.Core.Transformation.Optimize.SimplifyComparisons
import Juvix.Compiler.Core.Transformation.Optimize.SimplifyIfs
import Juvix.Compiler.Core.Transformation.Optimize.SpecializeArgs

optimize' :: forall r. (Members '[Reader CoreOptions, Dumper] r) => CoreOptions -> Module -> Sem r Module
optimize' opts@CoreOptions {..} md = do
  pure (letFolding md)
    >>= dump "let_folding"
    >>= doConstantFolding
    >>= dump "constant_folding"
    >>= composeM
      (4 * _optOptimizationLevel)
      ( doInlining
          >=> dump "inlining"
          >=> doSimplification 2
          >=> dump "simplification"
          >=> pure
          . specializeArgs
          >=> dump "specialize_args"
          >=> doSimplification 1
          >=> dump "simplification"
          >=> doConstantFolding
          >=> dump "constant_folding"
      )
    >>= pure
    . filterUnreachable
  where
    tab :: InfoTable
    tab = computeCombinedInfoTable md

    nonRecs :: HashSet Symbol
    nonRecs = nonRecursiveIdents' tab

    nonRecsReachable :: HashSet Symbol
    nonRecsReachable = nonRecursiveReachableIdents' tab

    symOcc :: HashMap Symbol Int
    symOcc = getTableSymbolsMap tab

    doConstantFolding :: Module -> Sem r Module
    doConstantFolding md' = pure $ constantFolding' opts nonRecs' tab' md'
      where
        tab' = computeCombinedInfoTable md'
        nonRecs'
          | _optOptimizationLevel > 1 = nonRecursiveReachableIdents' tab'
          | otherwise = nonRecsReachable

    doInlining :: Module -> Sem r Module
    doInlining md' = pure $ inlining' _optInliningDepth nonRecs' symOcc md'
      where
        nonRecs' =
          if
            | _optOptimizationLevel > 1 -> nonRecursiveIdents md'
            | otherwise -> nonRecs

    doSimplification :: Int -> Module -> Sem r Module
    doSimplification n =
      pure
        . simplifyArithmetic
        . simplifyIfs' (_optOptimizationLevel <= 1)
        . simplifyComparisons
        . caseFolding
        . casePermutation
        . compose n (letFolding' (isInlineableLambda _optInliningDepth))
        . lambdaFolding

optimize :: (Members '[Reader CoreOptions, Dumper] r) => Module -> Sem r Module
optimize tab = do
  opts <- ask
  optimize' opts tab
