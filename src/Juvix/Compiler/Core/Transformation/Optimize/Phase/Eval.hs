module Juvix.Compiler.Core.Transformation.Optimize.Phase.Eval where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding

optimize :: Member (Reader CoreOptions) r => InfoTable -> Sem r InfoTable
optimize =
  withOptimizationLevel 1 $
    return . letFolding . lambdaFolding
