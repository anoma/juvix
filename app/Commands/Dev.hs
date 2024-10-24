module Commands.Dev
  ( module Commands.Dev,
    module Commands.Dev.Options,
  )
where

import Commands.Base
import Commands.Dev.Anoma qualified as Anoma
import Commands.Dev.Asm qualified as Asm
import Commands.Dev.Casm qualified as Casm
import Commands.Dev.Core qualified as Core
import Commands.Dev.DevCompile qualified as DevCompile
import Commands.Dev.DisplayRoot qualified as DisplayRoot
import Commands.Dev.Highlight qualified as Highlight
import Commands.Dev.ImportTree qualified as ImportTree
import Commands.Dev.Internal qualified as Internal
import Commands.Dev.Latex qualified as Latex
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

runCommand :: (Members AppEffects r) => DevCommand -> Sem r ()
runCommand = \case
  ImportTree opts -> ImportTree.runCommand opts
  Latex opts -> Latex.runCommand opts
  Highlight opts -> Highlight.runCommand opts
  DevCompile opts -> DevCompile.runCommand opts
  Parse opts -> Parse.runCommand opts
  Scope opts -> Scope.runCommand opts
  Internal opts -> Internal.runCommand opts
  Termination opts -> Termination.runCommand opts
  Core opts -> Core.runCommand opts
  Asm opts -> Asm.runCommand opts
  Reg opts -> Reg.runCommand opts
  Tree opts -> Tree.runCommand opts
  Casm opts -> Casm.runCommand opts
  Runtime opts -> Runtime.runCommand opts
  DisplayRoot opts -> DisplayRoot.runCommand opts
  JuvixDevRepl opts -> Repl.runCommand opts
  MigrateJuvixYaml opts -> runFilesIO $ MigrateJuvixYaml.runCommand opts
  Nockma opts -> Nockma.runCommand opts
  Anoma opts -> Anoma.runCommand opts
