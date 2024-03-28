module Commands.CompileNew.Wasi where

import Commands.Base
import Commands.CompileNew.NativeWasiHelper as Helper
import Commands.CompileNew.Wasi.Options
import Juvix.Compiler.Backend

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => WasiOptions -> Sem r ()
runCommand opts =
  Helper.runCommand
    HelperOptions
      { _helperCStage = opts ^. wasiCStage,
        _helperTarget = TargetCWasm32Wasi,
        _helperCompileCommonOptions = opts ^. wasiCompileCommonOptions,
        _helperDefaultOutputFile = \inputFile baseOutputFile ->
          case opts ^. wasiCStage of
            CSource -> replaceExtension' cFileExt inputFile
            CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
            CAssembly -> replaceExtension' ".wat" inputFile
            CExecutable -> removeExtension' baseOutputFile
      }
