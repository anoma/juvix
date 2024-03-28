module Commands.CompileNew.Native where

import Commands.Base
import Commands.CompileNew.Native.Options
import Commands.CompileNew.NativeWasiHelper as Helper
import Juvix.Compiler.Backend

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => NativeOptions -> Sem r ()
runCommand opts =
  Helper.runCommand
    HelperOptions
      { _helperCStage = opts ^. nativeCStage,
        _helperTarget = TargetCNative64,
        _helperCompileCommonOptions = opts ^. nativeCompileCommonOptions,
        _helperDefaultOutputFile = \inputFile baseOutputFile ->
          case opts ^. nativeCStage of
            CSource -> replaceExtension' cFileExt inputFile
            CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
            CAssembly -> replaceExtension' ".s" inputFile
            CExecutable -> removeExtension' baseOutputFile
      }
