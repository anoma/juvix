module Commands.Compile where

import Commands.Base
import Commands.Compile.Options
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.C.Translation.FromInternal qualified as MiniC
import System.Environment
import System.Process qualified as P

runCommand :: (Members '[Embed IO, App] r) => CompileOptions -> Sem r ()
runCommand opts@CompileOptions {..} = do
  miniC <- (^. MiniC.resultCCode) <$> runPipeline _compileInputFile upToMiniC
  inputFile <- someBaseToAbs' (_compileInputFile ^. pathPath)
  result <- runCompile inputFile opts miniC
  case result of
    Left err -> printFailureExit err
    _ -> return ()

inputCFile :: (Members '[App] r) => Path Abs File -> Sem r (Path Abs File)
inputCFile inputFileCompile = do
  buildDir <- askBuildDir
  return (buildDir <//> outputMiniCFile)
  where
    outputMiniCFile :: Path Rel File
    outputMiniCFile = replaceExtension' ".c" (filename inputFileCompile)

runCompile :: (Members '[Embed IO, App] r) => Path Abs File -> CompileOptions -> Text -> Sem r (Either Text ())
runCompile inputFileCompile o minic = do
  buildDir <- askBuildDir
  ensureDir buildDir
  f <- inputCFile inputFileCompile
  embed (TIO.writeFile (toFilePath f) minic)
  prepareRuntime o
  case o ^. compileTarget of
    TargetWasm -> runError (clangCompile inputFileCompile o)
    TargetC -> return (Right ())
    TargetNative -> runError (clangNativeCompile inputFileCompile o)

prepareRuntime :: forall r. (Members '[Embed IO, App] r) => CompileOptions -> Sem r ()
prepareRuntime o = mapM_ writeRuntime runtimeProjectDir
  where
    runtimeProjectDir :: [(Path Rel File, BS.ByteString)]
    runtimeProjectDir = case o ^. compileTarget of
      TargetNative -> libcRuntime
      _ -> case o ^. compileRuntime of
        RuntimeWasiStandalone -> wasiStandaloneRuntimeDir <> builtinCRuntimeDir <> wallocDir
        RuntimeWasiLibC -> libcRuntime
        RuntimeStandalone -> standaloneRuntimeDir <> builtinCRuntimeDir <> wallocDir

    writeRuntime :: (Path Rel File, BS.ByteString) -> Sem r ()
    writeRuntime (filePath, contents) = do
      buildDir <- askBuildDir
      embed (BS.writeFile (toFilePath (buildDir <//> filePath)) contents)

wasiStandaloneRuntimeDir :: [(Path Rel File, BS.ByteString)]
wasiStandaloneRuntimeDir = map (first relFile) $(FE.makeRelativeToProject "c-runtime/wasi-standalone" >>= FE.embedDir)

standaloneRuntimeDir :: [(Path Rel File, BS.ByteString)]
standaloneRuntimeDir = map (first relFile) $(FE.makeRelativeToProject "c-runtime/standalone" >>= FE.embedDir)

wasiLibCRuntimeDir :: [(Path Rel File, BS.ByteString)]
wasiLibCRuntimeDir = map (first relFile) $(FE.makeRelativeToProject "c-runtime/wasi-libc" >>= FE.embedDir)

builtinCRuntimeDir :: [(Path Rel File, BS.ByteString)]
builtinCRuntimeDir = map (first relFile) $(FE.makeRelativeToProject "c-runtime/builtins" >>= FE.embedDir)

wallocDir :: [(Path Rel File, BS.ByteString)]
wallocDir = map (first relFile) $(FE.makeRelativeToProject "c-runtime/walloc" >>= FE.embedDir)

libcRuntime :: [(Path Rel File, BS.ByteString)]
libcRuntime = wasiLibCRuntimeDir <> builtinCRuntimeDir

clangNativeCompile ::
  forall r.
  (Members '[Embed IO, App, Error Text] r) =>
  Path Abs File ->
  CompileOptions ->
  Sem r ()
clangNativeCompile inputFileCompile o = do
  inputFile <- getInputFile
  outputFile <- getOutputFile
  buildDir <- askBuildDir
  runClang (nativeArgs buildDir outputFile inputFile)
  where
    getOutputFile :: Sem r (Path Abs File)
    getOutputFile = case o ^. compileOutputFile of
      Nothing -> return (removeExtension' inputFileCompile)
      Just f -> someBaseToAbs' (f ^. pathPath)

    getInputFile :: Sem r (Path Abs File)
    getInputFile = inputCFile inputFileCompile

clangCompile ::
  forall r.
  (Members '[Embed IO, App, Error Text] r) =>
  Path Abs File ->
  CompileOptions ->
  Sem r ()
clangCompile inputFileCompile o = do
  outputFile <- getOutputFile
  inputFile <- getInputFile
  let clangArgs :: Sem r [String]
      clangArgs = case o ^. compileRuntime of
        RuntimeStandalone -> do
          standaloneLibArgs outputFile inputFile
        RuntimeWasiStandalone -> wasiStandaloneArgs outputFile inputFile
        RuntimeWasiLibC -> wasiLibcArgs outputFile inputFile

  clangArgs >>= runClang
  where
    getOutputFile :: Sem r (Path Abs File)
    getOutputFile = maybe (return defaultOutputFile) someBaseToAbs' (o ^? compileOutputFile . _Just . pathPath)

    defaultOutputFile :: Path Abs File
    defaultOutputFile = replaceExtension' ".wasm" inputFileCompile

    getInputFile :: Sem r (Path Abs File)
    getInputFile = inputCFile inputFileCompile

sysrootEnvVar :: (Members '[Error Text, Embed IO] r) => Sem r (Path Abs Dir)
sysrootEnvVar =
  absDir
    <$> fromMaybeM (throw msg) (embed (lookupEnv "WASI_SYSROOT_PATH"))
  where
    msg :: Text
    msg = "Missing environment variable WASI_SYSROOT_PATH"

commonArgs :: Path Abs Dir -> Path Abs File -> [String]
commonArgs buildDir wasmOutputFile =
  [ "-std=c99",
    "-Oz",
    "-I",
    toFilePath buildDir,
    "-o",
    toFilePath wasmOutputFile
  ]

standaloneLibArgs :: (Members '[App, Embed IO] r) => Path Abs File -> Path Abs File -> Sem r [String]
standaloneLibArgs wasmOutputFile inputFile = do
  buildDir <- askBuildDir
  return $
    commonArgs buildDir wasmOutputFile
      <> [ "--target=wasm32",
           "-nodefaultlibs",
           "-nostartfiles",
           "-Wl,--no-entry",
           toFilePath (buildDir <//> $(mkRelFile "walloc.c")),
           toFilePath inputFile
         ]

wasiStandaloneArgs :: (Members '[App, Error Text, Embed IO] r) => Path Abs File -> Path Abs File -> Sem r [String]
wasiStandaloneArgs wasmOutputFile inputFile = do
  buildDir <- askBuildDir
  com <- wasiCommonArgs wasmOutputFile
  return $
    com
      <> [ toFilePath (buildDir <//> $(mkRelFile "walloc.c")),
           toFilePath inputFile
         ]

wasiLibcArgs :: (Members '[App, Error Text, Embed IO] r) => Path Abs File -> Path Abs File -> Sem r [String]
wasiLibcArgs wasmOutputFile inputFile = do
  com <- wasiCommonArgs wasmOutputFile
  return $ com <> ["-lc", toFilePath inputFile]

nativeArgs :: Path Abs Dir -> Path Abs File -> Path Abs File -> [String]
nativeArgs buildDir outputFile inputFile =
  commonArgs buildDir outputFile <> [toFilePath inputFile]

wasiCommonArgs :: (Members '[App, Error Text, Embed IO] r) => Path Abs File -> Sem r [String]
wasiCommonArgs wasmOutputFile = do
  sysrootPath <- sysrootEnvVar
  buildDir <- askBuildDir
  return $
    commonArgs buildDir wasmOutputFile
      <> [ "-nodefaultlibs",
           "--target=wasm32-wasi",
           "--sysroot",
           toFilePath sysrootPath
         ]

runClang ::
  (Members '[Embed IO, Error Text] r) =>
  [String] ->
  Sem r ()
runClang args = do
  (exitCode, _, err) <- embed (P.readProcessWithExitCode "clang" args "")
  case exitCode of
    ExitSuccess -> return ()
    _ -> throw (pack err)
