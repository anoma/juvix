module Juvix.Compiler.Core.Transformation.Optimize.Phase.Main where

import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.CaseFolding
import Juvix.Compiler.Core.Transformation.Optimize.CasePermutation
import Juvix.Compiler.Core.Transformation.Optimize.FilterUnreachable
import Juvix.Compiler.Core.Transformation.Optimize.Inlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.SimplifyComparisons
import Juvix.Compiler.Core.Transformation.Optimize.SimplifyIfs
import Juvix.Compiler.Core.Transformation.Optimize.SpecializeArgs

optimize' :: CoreOptions -> InfoTable -> InfoTable
optimize' CoreOptions {..} tab =
  filterUnreachable
    . compose
      (4 * _optOptimizationLevel)
      ( doSimplification 2
          . doInlining
          . doSimplification 1
          . specializeArgs
      )
    . letFolding
    $ tab
  where
    recs :: HashSet Symbol
    recs = recursiveIdents tab

    doInlining :: InfoTable -> InfoTable
    doInlining tab' = inlining' _optInliningDepth recs' tab'
      where
        recs' =
          if
              | _optOptimizationLevel > 1 -> recursiveIdents tab'
              | otherwise -> recs

    doSimplification :: Int -> InfoTable -> InfoTable
    doSimplification n =
      simplifyIfs' (_optOptimizationLevel <= 1)
        . simplifyComparisons
        . caseFolding
        . casePermutation
        . compose n (letFolding' (isInlineableLambda _optInliningDepth))
        . lambdaFolding

optimize :: (Member (Reader CoreOptions) r) => InfoTable -> Sem r InfoTable
optimize tab = do
  opts <- ask
  return $ optimize' opts tab
