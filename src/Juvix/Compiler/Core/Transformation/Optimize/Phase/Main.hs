module Juvix.Compiler.Core.Transformation.Optimize.Phase.Main where

import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.CaseFolding
import Juvix.Compiler.Core.Transformation.Optimize.FilterUnreachable
import Juvix.Compiler.Core.Transformation.Optimize.Inlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.SpecializeArgs

optimize' :: CoreOptions -> InfoTable -> InfoTable
optimize' CoreOptions {..} tab =
  filterUnreachable
    . compose
      (4 * _optOptimizationLevel)
      ( compose 2 (letFolding' (isInlineableLambda _optInliningDepth))
          . lambdaFolding
          . inlining' _optInliningDepth (recursiveIdents tab)
          . caseFolding
          . letFolding' (isInlineableLambda _optInliningDepth)
          . lambdaFolding
          . specializeArgs
      )
    . letFolding
    $ tab

optimize :: Member (Reader CoreOptions) r => InfoTable -> Sem r InfoTable
optimize tab = do
  opts <- ask
  return $ optimize' opts tab
