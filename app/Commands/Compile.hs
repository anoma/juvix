module Commands.Compile
  ( module Commands.Compile,
    module Commands.Compile.Options,
  )
where

import Commands.Base
import Commands.Compile.Anoma qualified as Anoma
import Commands.Compile.Cairo qualified as Cairo
import Commands.Compile.Native qualified as Native
import Commands.Compile.Options
import Commands.Compile.RiscZeroRust qualified as RiscZeroRust
import Commands.Compile.Vampir qualified as Vampir
import Commands.Compile.Wasi qualified as Wasi

runCommand :: (Members AppEffects r) => CompileCommand -> Sem r ()
runCommand = \case
  Native opts -> Native.runCommand opts
  Wasi opts -> Wasi.runCommand opts
  Anoma opts -> Anoma.runCommand opts
  Cairo opts -> Cairo.runCommand opts
  Vampir opts -> Vampir.runCommand opts
  RiscZeroRust opts -> RiscZeroRust.runCommand opts
