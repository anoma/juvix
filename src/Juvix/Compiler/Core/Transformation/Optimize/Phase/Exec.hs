module Juvix.Compiler.Core.Transformation.Optimize.Phase.Exec where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.LambdaLetRecLifting
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.Phase.Main qualified as Main
import Juvix.Compiler.Core.Transformation.TopEtaExpand
import Juvix.Compiler.Verification.Dumper (Dumper)

optimize :: (Members '[Reader CoreOptions, Dumper] r) => Module -> Sem r Module
optimize tab = do
  withOptimizationLevel' tab 1 $ \md ->
    topEtaExpand
      . letFolding
      . lambdaLetRecLifting
      <$> Main.optimize md
