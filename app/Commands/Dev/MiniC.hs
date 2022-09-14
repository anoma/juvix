module Commands.Dev.MiniC where

import Commands.Base
import Commands.Dev.MiniC.Options
import Juvix.Compiler.Backend.C.Translation.FromInternal qualified as MiniC

runCommand :: Members '[Embed IO, App] r => MiniCOptions -> Sem r ()
runCommand opts = do
  miniC <- (^. MiniC.resultCCode) <$> runPipeline (opts ^. miniCInputFile) upToMiniC
  say miniC
