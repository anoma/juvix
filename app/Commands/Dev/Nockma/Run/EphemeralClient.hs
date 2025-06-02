module Commands.Dev.Nockma.Run.EphemeralClient where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Run.Anoma
import Commands.Dev.Nockma.Run.EphemeralClient.Options

runCommand :: forall r. (Members AppEffects r) => NockmaRunEphemeralClientOptions -> Sem r ()
runCommand opts = do
  anomaDir <- AnomaPath <$> fromAppPathDir (opts ^. nockmaRunEphemeralAnomaDir)
  runAppError @SimpleError
    . runAnomaEphemeral anomaDir
    $ runInAnoma runArgs
  where
    runArgs =
      RunCommandArgs
        { _runCommandProgramFile = opts ^. nockmaRunEphemeralFile,
          _runCommandArgs = opts ^. nockmaRunEphemeralArgs
        }
