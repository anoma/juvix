module Commands.Dependencies.Update where

import Commands.Base

runCommand :: (Members '[Embed IO, App] r) => Sem r ()
runCommand = runPipelineNoFile (upToSetup (set dependenciesConfigForceUpdateLockfile True defaultDependenciesConfig))
