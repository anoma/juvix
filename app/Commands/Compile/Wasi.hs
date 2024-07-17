module Commands.Compile.Wasi where

import Commands.Base
import Commands.Compile.NativeWasiHelper as Helper
import Commands.Compile.Wasi.Options

runCommand ::
  forall r.
  (Members AppEffects r) =>
  WasiOptions 'InputMain ->
  Sem r ()
runCommand = Helper.runCommand . wasiHelperOptions
