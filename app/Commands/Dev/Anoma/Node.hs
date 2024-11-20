module Commands.Dev.Anoma.Node where

import Anoma.Effect
import Commands.Base
import Commands.Dev.Anoma.Node.Options

runCommand :: forall r. (Members AppEffects r) => NodeOptions -> Sem r ()
runCommand opts = runAppError @SimpleError
  . runConcurrent
  . runProcess
  $ do
    let launchMode = opts ^. nodeLaunchMode
    anomaDir :: AnomaPath <- AnomaPath <$> fromAppPathDir (opts ^. nodeAnomaPath)
    processH <- launchAnomaClient launchMode anomaDir
    case launchMode of
      LaunchModeAttached -> void (waitForProcess processH)
      LaunchModeDetached -> return ()
