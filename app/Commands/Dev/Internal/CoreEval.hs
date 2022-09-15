module Commands.Dev.Internal.CoreEval where

import Commands.Base
import Commands.Dev.Internal.CoreEval.Options
import Evaluator
import Juvix.Compiler.Core.Translation

runCommand :: Members '[Embed IO, App] r => InternalCoreEvalOptions -> Sem r ()
runCommand localOpts = do
  res <- runPipeline (localOpts ^. internalCoreEvalInputFile) upToCore
  mapM_ (evalNode localOpts (res ^. coreResultTable)) (res ^. coreResultNode)
