module Commands.Dependencies.Update where

import Commands.Base

runCommand :: (Members '[Embed IO, TaggedLock, App] r) => Sem r ()
runCommand = runPipelineNoFile (upToSetup (set dependenciesConfigForceUpdateLockfile True defaultDependenciesConfig))
