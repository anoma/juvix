module Commands.Dependencies.Update where

import Commands.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver

runCommand :: (Members AppEffects r) => Sem r ()
runCommand = do
  let opts = set (pipelineDependenciesConfig . dependenciesConfigForceUpdateLockfile) True defaultPipelineOptions
  runReader opts . runPipelineSetup $ return ()
