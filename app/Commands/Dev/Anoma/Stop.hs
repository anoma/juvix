module Commands.Dev.Anoma.Stop where

import Anoma.Client.Config
import Commands.Base
import Commands.Dev.Anoma.Client

runCommand :: forall r. (Members AppEffects r) => Sem r ()
runCommand = runAppError @SimpleError $ do
  mconfig <- checkClientRunning
  case mconfig of
    Just config -> stopClient config >> removeConfig
    Nothing -> logInfo "The Anoma client is not running" >> exitFailure
