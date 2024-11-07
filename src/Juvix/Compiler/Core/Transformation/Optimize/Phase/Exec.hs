module Juvix.Compiler.Core.Transformation.Optimize.Phase.Exec where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Scoper
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.Phase.Main qualified as Main
import Juvix.Compiler.Core.Transformation.TopEtaExpand

optimize :: (Member (Reader CoreOptions) r) => Module -> Sem r Module
optimize tab = do
  opts <- ask
  withOptimizationLevel' tab 1 $
    return
      . scopeCheckDebug
      . topEtaExpand
      . scopeCheckDebug
      -- . letFolding
      . scopeCheckDebug
      -- . lambdaLetRecLifting
      . scopeCheckDebug
      . Main.optimize' opts
      . scopeCheckDebug
