module Commands.CompileNew
  ( module Commands.CompileNew,
    module Commands.CompileNew.Options,
  )
where

import Commands.Base
import Commands.CompileNew.Anoma qualified as Anoma
import Commands.CompileNew.Casm qualified as Casm
import Commands.CompileNew.Geb qualified as Geb
import Commands.CompileNew.Native qualified as Native
import Commands.CompileNew.Options
import Commands.CompileNew.Vampir qualified as Vampir
import Commands.CompileNew.Wasi qualified as Wasi

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => CompileNewCommand -> Sem r ()
runCommand = \case
  Native opts -> Native.runCommand opts
  Wasi opts -> Wasi.runCommand opts
  Geb opts -> Geb.runCommand opts
  Anoma opts -> Anoma.runCommand opts
  Casm opts -> Casm.runCommand opts
  Vampir opts -> Vampir.runCommand opts
