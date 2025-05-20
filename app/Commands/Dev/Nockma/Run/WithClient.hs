module Commands.Dev.Nockma.Run.WithClient where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Run.Anoma
import Commands.Dev.Nockma.Run.WithClient.Options

runCommand :: forall r. (Members AppEffects r) => NockmaRunWithClientOptions -> Sem r ()
runCommand opts =
  runAppError @SimpleError
    . runAnomaWithClient anomaInfo
    $ runInAnoma runArgs
  where
    anomaInfo =
      AnomaClientInfo
        { _anomaClientInfoUrl = opts ^. nockmaRunWithClientUrl,
          _anomaClientInfoPort = opts ^. nockmaRunWithClientPort,
          _anomaClientInfoNodeId = opts ^. nockmaRunWithClientNodeId
        }
    runArgs =
      RunCommandArgs
        { _runCommandProgramFile = opts ^. nockmaRunWithClientFile,
          _runCommandArgs = opts ^. nockmaRunWithClientArgs
        }
