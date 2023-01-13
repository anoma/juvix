module Commands.Dev
  ( module Commands.Dev,
    module Commands.Dev.Options,
  )
where

import Commands.Base
import Commands.Dev.Asm qualified as Asm
import Commands.Dev.Core qualified as Core
import Commands.Dev.DisplayRoot qualified as DisplayRoot
import Commands.Dev.Highlight qualified as Highlight
import Commands.Dev.Internal qualified as Internal
import Commands.Dev.MiniC qualified as MiniC
import Commands.Dev.Options
import Commands.Dev.Parse qualified as Parse
import Commands.Dev.Runtime qualified as Runtime
import Commands.Dev.Scope qualified as Scope
import Commands.Dev.Termination qualified as Termination

runCommand :: Members '[Embed IO, App] r => DevCommand -> Sem r ()
runCommand = \case
  Highlight opts -> Highlight.runCommand opts
  Parse opts -> Parse.runCommand opts
  Scope opts -> Scope.runCommand opts
  Internal opts -> Internal.runCommand opts
  MiniC opts -> MiniC.runCommand opts
  Termination opts -> Termination.runCommand opts
  Core opts -> Core.runCommand opts
  Asm opts -> Asm.runCommand opts
  Runtime opts -> Runtime.runCommand opts
  DisplayRoot opts -> DisplayRoot.runCommand opts
