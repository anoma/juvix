module Commands.Dev.InstanceTermination where

import Commands.Base
import Commands.Dev.InstanceTermination.Calls qualified as Calls
import Commands.Dev.InstanceTermination.Options

runCommand :: (Members AppEffects r) => InstanceTerminationCommand -> Sem r ()
runCommand = \case
  Calls opts -> Calls.runCommand opts
