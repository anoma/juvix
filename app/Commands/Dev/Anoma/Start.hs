module Commands.Dev.Anoma.Start where

import Anoma.Effect
import Commands.Base
import Commands.Dev.Anoma.Start.Options

runCommand :: forall r. (Members AppEffects r) => StartOptions -> Sem r ()
runCommand opts = runAppError @SimpleError
  . runConcurrent
  . runProcess
  $ do
    let launchMode = opts ^. startLaunchMode
    anomaDir :: AnomaPath <- AnomaPath <$> fromAppPathDir (opts ^. startAnomaPath)
    processH <- launchAnomaClient launchMode anomaDir
    case launchMode of
      LaunchModeAttached -> void (waitForProcess processH)
      LaunchModeDetached -> return ()
