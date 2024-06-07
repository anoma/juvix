module Commands.Dev.DevCompile.NativeRust where

import Commands.Base
import Commands.Dev.DevCompile.NativeRust.Options
import Commands.Extra.Rust
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Compiler.Backend.Rust.Data.Result

runCommand ::
  (Members '[App, EmbedIO, TaggedLock] r) =>
  NativeRustOptions 'InputMain ->
  Sem r ()
runCommand opts = do
  let opts' = opts ^. nativeRustCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputFile = opts' ^. compileOutputFile
  mainFile <- getMainFile inputFile
  Result {..} <- runPipeline opts inputFile upToRust
  rustFile <- inputRustFile mainFile
  writeFileEnsureLn rustFile _resultRustCode
  buildDir <- askBuildDir
  ensureDir buildDir
  prepareRuntime
  outputFile <- nativeOutputFile mainFile moutputFile
  let args =
        RustArgs
          { _rustDebug = opts' ^. compileDebug,
            _rustInputFile = rustFile,
            _rustOutputFile = outputFile,
            _rustOptimizationLevel = fmap (min 3 . (+ 1)) (opts' ^. compileOptimizationLevel)
          }
  rustCompile args
  where
    prepareRuntime ::
      forall s.
      (Members '[App, EmbedIO] s) =>
      Sem s ()
    prepareRuntime = writeRuntime runtime
      where
        runtime :: BS.ByteString
        runtime
          | opts ^. nativeRustCompileCommonOptions . compileDebug = rustDebugRuntime
          | otherwise = rustReleaseRuntime
          where
            rustReleaseRuntime :: BS.ByteString
            rustReleaseRuntime = $(FE.makeRelativeToProject "runtime/rust/juvix/target/release/libjuvix.rlib" >>= FE.embedFile)

            rustDebugRuntime :: BS.ByteString
            rustDebugRuntime = $(FE.makeRelativeToProject "runtime/rust/juvix/target/debug/libjuvix.rlib" >>= FE.embedFile)

inputRustFile :: (Members '[App, EmbedIO] r) => Path Abs File -> Sem r (Path Abs File)
inputRustFile inputFileCompile = do
  buildDir <- askBuildDir
  ensureDir buildDir
  return (buildDir <//> replaceExtension' ".rs" (filename inputFileCompile))

nativeOutputFile :: forall r. (Member App r) => Path Abs File -> Maybe (AppPath File) -> Sem r (Path Abs File)
nativeOutputFile inputFile moutputFile =
  case moutputFile of
    Just f -> fromAppFile f
    Nothing -> do
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return $ removeExtension' baseOutputFile

writeRuntime ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  BS.ByteString ->
  Sem r ()
writeRuntime runtime = do
  buildDir <- askBuildDir
  liftIO $
    BS.writeFile (toFilePath (buildDir <//> $(mkRelFile "libjuvix.rlib"))) runtime
