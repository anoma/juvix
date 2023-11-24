module Juvix.Compiler.Core.Transformation.Optimize.Phase.Eval where

import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.CaseFolding
import Juvix.Compiler.Core.Transformation.Optimize.CaseValueInlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.MandatoryInlining

optimize :: Module -> Sem r Module
optimize =
  return
    . letFolding
    . lambdaFolding
    . letFolding
    . caseFolding
    . caseValueInlining
    . letFolding
    . lambdaFolding
    . mandatoryInlining
