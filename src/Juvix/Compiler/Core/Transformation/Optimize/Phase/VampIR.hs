module Juvix.Compiler.Core.Transformation.Optimize.Phase.VampIR where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.CaseCallLifting
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.SimplifyIfs

optimize :: (Member (Reader CoreOptions) r) => InfoTable -> Sem r InfoTable
optimize =
  withOptimizationLevel 1 $
    return . letFolding . simplifyIfs . caseCallLifting . letFolding . lambdaFolding
