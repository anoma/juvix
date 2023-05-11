module Juvix.Compiler.Core.Transformation.Optimize.Phase.Exec where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.Phase.Main qualified as Main

optimize :: Member (Reader CoreOptions) r => InfoTable -> Sem r InfoTable
optimize tab = do
  opts <- ask
  withOptimizationLevel' tab 1 $
    return
      . letFolding
      . lambdaLetRecLifting
      . Main.optimize' opts
