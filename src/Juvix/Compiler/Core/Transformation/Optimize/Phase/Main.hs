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
optimize' CoreOptions {..} md =
  filterUnreachable
    . compose
      (4 * _optOptimizationLevel)
      ( doConstantFolding
          . doSimplification 2
          . doInlining
          . doSimplification 1
          . specializeArgs
      )
    . doConstantFolding
    . letFolding
    $ md
  where
    tab :: InfoTable
    tab = computeCombinedInfoTable md

    recs :: HashSet Symbol
    recs = recursiveIdents' tab

    nonRecs :: HashSet Symbol
    nonRecs = nonRecursiveIdents' tab

    doConstantFolding :: Module -> Module
    doConstantFolding md' = constantFolding' nonRecs' tab' md'
      where
        tab' = computeCombinedInfoTable md'
        nonRecs'
          | _optOptimizationLevel > 1 = nonRecursiveIdents' tab'
          | otherwise = nonRecs

    doInlining :: Module -> Module
    doInlining md' = inlining' _optInliningDepth recs' md'
      where
        recs' =
          if
              | _optOptimizationLevel > 1 -> recursiveIdents md'
              | otherwise -> recs

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
