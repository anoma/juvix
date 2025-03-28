module Commands.Dev.Tree.Compile where

import Commands.Base
import Commands.Dev.Tree.Compile.Anoma qualified as Anoma
import Commands.Dev.Tree.Compile.Asm qualified as Asm
import Commands.Dev.Tree.Compile.Cairo qualified as Cairo
import Commands.Dev.Tree.Compile.Casm qualified as Casm
import Commands.Dev.Tree.Compile.Native qualified as Native
import Commands.Dev.Tree.Compile.Options
import Commands.Dev.Tree.Compile.Reg qualified as Reg
import Commands.Dev.Tree.Compile.RiscZeroRust qualified as RiscZeroRust
import Commands.Dev.Tree.Compile.Wasi qualified as Wasi

runCommand ::
  forall r.
  (Members '[EmbedIO, App, TaggedLock, Files] r) =>
  CompileCommand ->
  Sem r ()
runCommand = \case
  Native opts -> Native.runCommand opts
  Wasi opts -> Wasi.runCommand opts
  Asm opts -> Asm.runCommand opts
  Casm opts -> Casm.runCommand opts
  Reg opts -> Reg.runCommand opts
  Anoma opts -> Anoma.runCommand opts
  Cairo opts -> Cairo.runCommand opts
  RiscZeroRust opts -> RiscZeroRust.runCommand opts
