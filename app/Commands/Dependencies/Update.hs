module Commands.Dependencies.Update where

import Commands.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver

runCommand :: (Members '[Embed IO, App] r) => Sem r ()
runCommand = runPipelineNoFile (upToSetup (set dependenciesConfigForceUpdateLockfile True defaultDependenciesConfig))
