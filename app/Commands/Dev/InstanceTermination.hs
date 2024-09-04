module Commands.Dev.InstanceTermination where

import Commands.Base
import Commands.Dev.InstanceTermination.Calls qualified as Calls
import Commands.Dev.InstanceTermination.Options

runCommand :: InstanceTerminationCommand -> Sem r ()
runCommand = \case
  Calls opts -> Calls.runCommand opts
