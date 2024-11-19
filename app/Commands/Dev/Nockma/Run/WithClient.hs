module Commands.Dev.Nockma.Run.WithClient where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Run.Anoma
import Commands.Dev.Nockma.Run.WithClient.Options

runCommand :: forall r. (Members AppEffects r) => NockmaRunWithClientOptions -> Sem r ()
runCommand opts =
  runAppError @SimpleError
    . runAnomaWithClient grpcInfo
    $ runInAnoma runArgs
  where
    grpcInfo =
      AnomaGrpcClientInfo
        { _anomaGrpcClientInfoUrl = opts ^. nockmaRunWithClientUrl,
          _anomaGrpcClientInfoPort = opts ^. nockmaRunWithClientGrpcPort
        }
    runArgs =
      RunCommandArgs
        { _runCommandProgramFile = opts ^. nockmaRunWithClientFile,
          _runCommandArgsFile = opts ^. nockmaRunWithClientArgs
        }
