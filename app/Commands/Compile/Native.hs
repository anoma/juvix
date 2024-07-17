module Commands.Compile.Native where

import Commands.Base
import Commands.Compile.Native.Options
import Commands.Compile.NativeWasiHelper as Helper

runCommand ::
  forall r.
  (Members AppEffects r) =>
  NativeOptions 'InputMain ->
  Sem r ()
runCommand = Helper.runCommand . nativeHelperOptions
