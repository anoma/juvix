module Commands.Dev.Anoma.Status where

import Commands.Base
import Commands.Dev.Anoma.Client
import Juvix.Data.CodeAnn

runCommand :: forall r. (Members AppEffects r) => Sem r ()
runCommand = runAppError @SimpleError $ do
  mconfig <- checkClientRunning
  case mconfig of
    Just config -> renderStdOutLn (ppCodeAnn config)
    Nothing -> logInfo "The Anoma client is not running" >> exitFailure
