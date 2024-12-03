module Juvix.Compiler.Core.Transformation.Optimize.Phase.Main where

import Juvix.Compiler.Core.Data.IdentDependencyInfo
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

optimize' :: CoreOptions -> Module -> Module
optimize' opts@CoreOptions {..} md =
  filterUnreachable
    . compose
      (4 * _optOptimizationLevel)
      ( doConstantFolding
          . doSimplification 1
          . specializeArgs
          . doSimplification 2
          . doInlining
      )
    . doConstantFolding
    . letFolding
    $ md
  where
    tab :: InfoTable
    tab = computeCombinedInfoTable md

    nonRecs :: HashSet Symbol
    nonRecs = nonRecursiveIdents' tab

    nonRecsReachable :: HashSet Symbol
    nonRecsReachable = nonRecursiveReachableIdents' tab

    doConstantFolding :: Module -> Module
    doConstantFolding md' = constantFolding' opts nonRecs' tab' md'
      where
        tab' = computeCombinedInfoTable md'
        nonRecs'
          | _optOptimizationLevel > 1 = nonRecursiveReachableIdents' tab'
          | otherwise = nonRecsReachable

    doInlining :: Module -> Module
    doInlining md' = inlining' _optInliningDepth nonRecs' md'
      where
        nonRecs' =
          if
              | _optOptimizationLevel > 1 -> nonRecursiveIdents md'
              | otherwise -> nonRecs

    doSimplification :: Int -> Module -> Module
    doSimplification n =
      simplifyArithmetic
        . simplifyIfs' (_optOptimizationLevel <= 1)
        . simplifyComparisons
        . caseFolding
        . casePermutation
        . compose n (letFolding' (isInlineableLambda _optInliningDepth))
        . lambdaFolding

optimize :: (Member (Reader CoreOptions) r) => Module -> Sem r Module
optimize tab = do
  opts <- ask
  return $ optimize' opts tab
