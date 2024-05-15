{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.Native.Options
  ( module Commands.Compile.Native.Options,
    module Commands.Compile.CommonOptions,
    module Commands.Compile.CStage,
  )
where

import Commands.Base
import Commands.Compile.CStage
import Commands.Compile.CommonOptions
import Commands.Compile.NativeWasiHelper as Helper
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE

data NativeOptions (k :: InputKind) = NativeOptions
  { _nativeCompileCommonOptions :: CompileCommonOptions k,
    _nativeCStage :: CStage
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (NativeOptions k)

makeLenses ''NativeOptions

parseNative :: forall (k :: InputKind). (SingI k) => Parser (NativeOptions k)
parseNative = do
  _nativeCompileCommonOptions <- parseCompileCommonOptions
  _nativeCStage <- parseCStage
  pure NativeOptions {..}

nativeHelperOptions :: NativeOptions k -> Helper.HelperOptions k
nativeHelperOptions opts =
  Helper.HelperOptions
    { _helperCStage = opts ^. nativeCStage,
      _helperTarget = TargetCNative64,
      _helperCompileCommonOptions = opts ^. nativeCompileCommonOptions,
      _helperClangBackend = ClangNative,
      _helperDefaultOutputFile = nativeDefaultOutputFile,
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
            nativeReleaseRuntime = $(FE.makeRelativeToProject "runtime/c/_build.native64/libjuvix.a" >>= FE.embedFile)

            nativeDebugRuntime :: BS.ByteString
            nativeDebugRuntime = $(FE.makeRelativeToProject "runtime/c/_build.native64-debug/libjuvix.a" >>= FE.embedFile)

    nativeDefaultOutputFile :: Path Abs File -> Path Abs File -> Path Abs File
    nativeDefaultOutputFile inputFile baseOutputFile =
      case opts ^. nativeCStage of
        CSource -> replaceExtension' cFileExt inputFile
        CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
        CAssembly -> replaceExtension' ".s" inputFile
        CExecutable -> removeExtension' baseOutputFile

instance EntryPointOptions (NativeOptions k) where
  applyOptions opts = applyOptions (opts ^. nativeCompileCommonOptions)
