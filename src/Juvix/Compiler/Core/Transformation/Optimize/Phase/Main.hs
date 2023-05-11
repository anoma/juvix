module Juvix.Compiler.Core.Transformation.Optimize.Phase.Main where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.Inlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding

optimize' :: CoreOptions -> InfoTable -> InfoTable
optimize' CoreOptions {..} tab =
  compose
    (4 * _optOptimizationLevel)
    ( letFolding' (isInlineableLambda _optInliningDepth)
        . lambdaFolding
        . inlining' _optInliningDepth (recursiveIdents tab)
    )
    . letFolding
    $ tab

optimize :: Member (Reader CoreOptions) r => InfoTable -> Sem r InfoTable
optimize tab = do
  opts <- ask
  return $ optimize' opts tab
