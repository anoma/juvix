module Commands.Dev
  ( module Commands.Dev,
    module Commands.Dev.Options,
  )
where

import Commands.Base
import Commands.Dev.Asm qualified as Asm
import Commands.Dev.Casm qualified as Casm
import Commands.Dev.Core qualified as Core
import Commands.Dev.DisplayRoot qualified as DisplayRoot
import Commands.Dev.Geb qualified as Geb
import Commands.Dev.Highlight qualified as Highlight
import Commands.Dev.Internal qualified as Internal
import Commands.Dev.InternalCompile qualified as InternalCompile
import Commands.Dev.MigrateJuvixYaml qualified as MigrateJuvixYaml
import Commands.Dev.Nockma qualified as Nockma
import Commands.Dev.Options
import Commands.Dev.Parse qualified as Parse
import Commands.Dev.Reg qualified as Reg
import Commands.Dev.Runtime qualified as Runtime
import Commands.Dev.Scope qualified as Scope
import Commands.Dev.Termination qualified as Termination
import Commands.Dev.Tree qualified as Tree
import Commands.Repl qualified as Repl

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => DevCommand -> Sem r ()
runCommand = \case
  Highlight opts -> Highlight.runCommand opts
  InternalCompile opts -> InternalCompile.runCommand opts
  Parse opts -> Parse.runCommand opts
  Scope opts -> Scope.runCommand opts
  Internal opts -> Internal.runCommand opts
  Termination opts -> Termination.runCommand opts
  Core opts -> Core.runCommand opts
  Geb opts -> Geb.runCommand opts
  Asm opts -> Asm.runCommand opts
  Reg opts -> Reg.runCommand opts
  Tree opts -> Tree.runCommand opts
  Casm opts -> Casm.runCommand opts
  Runtime opts -> Runtime.runCommand opts
  DisplayRoot opts -> DisplayRoot.runCommand opts
  JuvixDevRepl opts -> Repl.runCommand opts
  MigrateJuvixYaml opts -> runFilesIO $ MigrateJuvixYaml.runCommand opts
  Nockma opts -> Nockma.runCommand opts
