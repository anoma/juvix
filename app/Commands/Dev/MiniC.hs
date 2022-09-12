module Commands.Dev.MiniC where

import Commands.Base
import Juvix.Compiler.Backend.C.Translation.FromInternal qualified as MiniC

runCommand :: Members '[Embed IO, App] r => EntryPoint -> Sem r ()
runCommand entryPoint = do
  miniC <- (^. MiniC.resultCCode) <$> runPipeline (upToMiniC entryPoint)
  say miniC
