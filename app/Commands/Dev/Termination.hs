module Commands.Dev.Termination where

import Commands.Base
import Commands.Dev.Termination.CallGraph qualified as CallGraph
import Commands.Dev.Termination.Calls qualified as Calls
import Commands.Dev.Termination.Options

runCommand :: Members '[Embed IO, App] r => TerminationCommand -> Sem r ()
runCommand = \case
  Calls opts -> Calls.runCommand opts
  CallGraph opts -> CallGraph.runCommand opts
