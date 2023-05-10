module Juvix.Compiler.Core.Transformation.Optimize.Phase.Exec where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
import Juvix.Compiler.Core.Transformation.Optimize.Inlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding

optimize :: Member (Reader CoreOptions) r => InfoTable -> Sem r InfoTable
optimize tab = do
  optLevel <- asks (^. optOptimizationLevel)
  withOptimizationLevel' tab 1 $
    return
      . letFolding
      . lambdaLetRecLifting
      . compose (3 * optLevel) (letFolding . lambdaFolding . inlining)
      . letFolding
