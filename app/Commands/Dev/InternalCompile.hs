module Commands.Dev.InternalCompile where

import Commands.Base
import Commands.Dev.InternalCompile.Options
import Commands.Dev.InternalCompile.Core qualified as Core
import Commands.Dev.InternalCompile.Asm qualified as Asm
import Commands.Dev.InternalCompile.Reg qualified as Reg
import Commands.Dev.InternalCompile.Nockma qualified as Nockma
import Commands.Dev.InternalCompile.Tree qualified as Tree
import Commands.Dev.InternalCompile.Cairo qualified as Cairo

runCommand :: InternalCompileCommand -> Sem r ()
runCommand = \case
  Core opts -> Core.runCommand opts
  Reg opts -> Reg.runCommand opts
  Asm opts -> Asm.runCommand opts
  Tree opts -> Tree.runCommand opts
  Nockma opts -> Nockma.runCommand opts
  Cairo opts -> Cairo.runCommand opts
