module Juvix.Compiler.Core.Transformation.Optimize.Phase.PreLifting where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.LoopHoisting

optimize :: (Member (Reader CoreOptions) r) => Module -> Sem r Module
optimize =
  withOptimizationLevel 1 $
    return
      . loopHoisting
      . letFolding
