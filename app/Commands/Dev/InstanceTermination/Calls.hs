module Commands.Dev.InstanceTermination.Calls where

import Commands.Base
import Commands.Dev.InstanceTermination.Calls.Options

runCommand :: CallsOptions -> Sem r ()
runCommand CallsOptions {} = return ()
