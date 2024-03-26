module Commands.Dev.DevCompile where

import Commands.Base
import Commands.Dev.DevCompile.Asm qualified as Asm
import Commands.Dev.DevCompile.Cairo qualified as Cairo
import Commands.Dev.DevCompile.Core qualified as Core
import Commands.Dev.DevCompile.Nockma qualified as Nockma
import Commands.Dev.DevCompile.Options
import Commands.Dev.DevCompile.Reg qualified as Reg
import Commands.Dev.DevCompile.Tree qualified as Tree

runCommand :: (Members '[App, EmbedIO, TaggedLock] r) => DevCompileCommand -> Sem r ()
runCommand = \case
  Core opts -> Core.runCommand opts
  Reg opts -> Reg.runCommand opts
  Asm opts -> Asm.runCommand opts
  Tree opts -> Tree.runCommand opts
  Nockma opts -> Nockma.runCommand opts
  Cairo opts -> Cairo.runCommand opts
