module Juvix.Compiler.Core.Transformation.Optimize.Phase.PreLifting where

import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.CaseFolding
import Juvix.Compiler.Core.Transformation.Optimize.Inlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding
import Juvix.Compiler.Core.Transformation.Optimize.LoopHoisting

optimize :: (Member (Reader CoreOptions) r) => Module -> Sem r Module
optimize md = do
  CoreOptions {..} <- ask
  withOptimizationLevel' md 1 $
    return
      . loopHoisting
      . letFolding
      . lambdaFolding
      . letFolding
      . caseFolding
      . compose
        2
        ( compose 2 (letFolding' (isInlineableLambda _optInliningDepth))
            . lambdaFolding
            . inlining' _optInliningDepth nonRecSyms symOcc
        )
      . letFolding
  where
    nonRecSyms = nonRecursiveIdents md
    symOcc = getModuleSymbolsMap md
