module Commands.Compile.Wasi where

import Commands.Base
import Commands.Compile.NativeWasiHelper as Helper
import Commands.Compile.Wasi.Options
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => WasiOptions -> Sem r ()
runCommand opts =
  Helper.runCommand
    HelperOptions
      { _helperCStage = opts ^. wasiCStage,
        _helperTarget = TargetCWasm32Wasi,
        _helperCompileCommonOptions = opts ^. wasiCompileCommonOptions,
        _helperClangBackend = ClangWasi,
        _helperPrepareRuntime = prepareRuntime,
        _helperDefaultOutputFile = \inputFile baseOutputFile ->
          case opts ^. wasiCStage of
            CSource -> replaceExtension' cFileExt inputFile
            CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
            CAssembly -> replaceExtension' ".wat" inputFile
            CExecutable -> replaceExtension' ".wasm" baseOutputFile
      }
  where
    prepareRuntime ::
      forall s.
      (Members '[App, EmbedIO] s) =>
      Sem s ()
    prepareRuntime = writeRuntime runtime
      where
        runtime :: BS.ByteString
        runtime
          | opts ^. wasiCompileCommonOptions . compileDebug = wasiDebugRuntime
          | otherwise = wasiReleaseRuntime
          where
            wasiReleaseRuntime :: BS.ByteString
            wasiReleaseRuntime = $(FE.makeRelativeToProject "runtime/_build.wasm32-wasi/libjuvix.a" >>= FE.embedFile)

            wasiDebugRuntime :: BS.ByteString
            wasiDebugRuntime = $(FE.makeRelativeToProject "runtime/_build.wasm32-wasi-debug/libjuvix.a" >>= FE.embedFile)
