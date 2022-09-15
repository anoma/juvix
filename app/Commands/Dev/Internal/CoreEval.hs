module Commands.Dev.Internal.CoreEval where

import Commands.Base
import Commands.Dev.Core.Eval
import Commands.Dev.Internal.CoreEval.Options
import Juvix.Compiler.Core.Translation

runCommand :: Members '[Embed IO, App] r => InternalCoreEvalOptions -> Sem r ()
runCommand localOpts = do
  res <- runPipeline inputFile upToCore
  mapM_ (evalNode (localOpts ^. internalCoreEvalNoIO) inputFile (res ^. coreResultTable)) (res ^. coreResultNode)
  where
    inputFile :: Path
    inputFile = localOpts ^. internalCoreEvalInputFile
