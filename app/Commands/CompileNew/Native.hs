module Commands.CompileNew.Native where

import Commands.Base
import Commands.CompileNew.Native.Options
import Commands.CompileNew.NativeWasiHelper as Helper
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Compiler.Backend

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => NativeOptions -> Sem r ()
runCommand opts =
  Helper.runCommand
    HelperOptions
      { _helperCStage = opts ^. nativeCStage,
        _helperTarget = TargetCNative64,
        _helperCompileCommonOptions = opts ^. nativeCompileCommonOptions,
        _helperClangBackend = ClangNative,
        _helperDefaultOutputFile = \inputFile baseOutputFile ->
          case opts ^. nativeCStage of
            CSource -> replaceExtension' cFileExt inputFile
            CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
            CAssembly -> replaceExtension' ".s" inputFile
            CExecutable -> removeExtension' baseOutputFile,
        _helperPrepareRuntime = prepareRuntime
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
          | opts ^. nativeCompileCommonOptions . compileDebug = nativeDebugRuntime
          | otherwise = nativeReleaseRuntime
          where
            nativeReleaseRuntime :: BS.ByteString
            nativeReleaseRuntime = $(FE.makeRelativeToProject "runtime/_build.native64/libjuvix.a" >>= FE.embedFile)

            nativeDebugRuntime :: BS.ByteString
            nativeDebugRuntime = $(FE.makeRelativeToProject "runtime/_build.native64-debug/libjuvix.a" >>= FE.embedFile)
