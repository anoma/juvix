{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.Wasi.Options
  ( module Commands.Compile.Wasi.Options,
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

data WasiOptions (k :: InputKind) = WasiOptions
  { _wasiCompileCommonOptions :: CompileCommonOptions k,
    _wasiCStage :: CStage
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (WasiOptions k)

makeLenses ''WasiOptions

parseWasi :: (SingI k) => Parser (WasiOptions k)
parseWasi = do
  _wasiCompileCommonOptions <- parseCompileCommonOptions
  _wasiCStage <- parseCStage
  pure WasiOptions {..}

wasiHelperOptions :: WasiOptions k -> Helper.HelperOptions k
wasiHelperOptions opts =
  Helper.HelperOptions
    { _helperCStage = opts ^. wasiCStage,
      _helperTarget = TargetCWasm32Wasi,
      _helperCompileCommonOptions = opts ^. wasiCompileCommonOptions,
      _helperClangBackend = ClangWasi,
      _helperPrepareRuntime = prepareRuntime,
      _helperDefaultOutputFile = wasiDefaultOutputFile
    }
  where
    prepareRuntime ::
      forall s.
      (Members '[App, EmbedIO] s) =>
      Sem s ()
    prepareRuntime = writeRuntime (fromJust runtime)
      where
        runtime :: Maybe BS.ByteString
        runtime
          | opts ^. wasiCompileCommonOptions . compileDebug = wasiDebugRuntime
          | otherwise = wasiReleaseRuntime
          where
            wasiReleaseRuntime :: Maybe BS.ByteString
            wasiReleaseRuntime = $(FE.makeRelativeToProject "runtime/c/_build.wasm32-wasi/libjuvix.a" >>= FE.embedFileIfExists)

            wasiDebugRuntime :: Maybe BS.ByteString
            wasiDebugRuntime = $(FE.makeRelativeToProject "runtime/c/_build.wasm32-wasi-debug/libjuvix.a" >>= FE.embedFileIfExists)

    wasiDefaultOutputFile :: Path Abs File -> Path Abs File -> Path Abs File
    wasiDefaultOutputFile inputFile baseOutputFile =
      case opts ^. wasiCStage of
        CSource -> replaceExtension' cFileExt inputFile
        CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
        CAssembly -> replaceExtension' ".wat" inputFile
        CExecutable -> replaceExtension' ".wasm" baseOutputFile

instance EntryPointOptions (WasiOptions k) where
  applyOptions opts = applyOptions (opts ^. wasiCompileCommonOptions)
