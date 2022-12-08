module Commands.Compile where

import Commands.Base
import Commands.Compile.Options
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.C.Translation.FromInternal qualified as MiniC
import Juvix.Extra.Paths
import System.Environment
import System.Process qualified as P

runCommand :: Members '[Embed IO, App] r => CompileOptions -> Sem r ()
runCommand opts@CompileOptions {..} = do
  root <- askRoot
  miniC <- (^. MiniC.resultCCode) <$> runPipeline _compileInputFile upToMiniC
  let inputFile = _compileInputFile ^. pathPath
  result <- embed (runCompile root inputFile opts miniC)
  case result of
    Left err -> printFailureExit err
    _ -> return ()

inputCFile :: Members '[App] r => Path Abs File -> Sem r (Path Abs File)
inputCFile inputFileCompile = do
  root <- askRoot
  return (projRoot <//> juvixBuildDir' <//> outputMiniCFile)
  where
    outputMiniCFile :: Path Rel File
    outputMiniCFile = replaceExtension' ".c" (filename inputFileCompile)

runCompile :: Members '[Embed IO, App] r => Path Abs File -> CompileOptions -> Text -> Sem r (Either Text ())
runCompile inputFileCompile o minic = do
  root <- askRoot
  ensureDir (root <//> juvixBuildDir')
  embed (TIO.writeFile (inputCFile projRoot inputFileCompile) minic)
  prepareRuntime o
  case o ^. compileTarget of
    TargetWasm -> runM (runError (clangCompile projRoot inputFileCompile o))
    TargetC -> return (Right ())
    TargetNative -> runM (runError (clangNativeCompile projRoot inputFileCompile o))

prepareRuntime :: Members '[Embed IO, App] r => CompileOptions -> IO ()
prepareRuntime o = mapM_ writeRuntime runtimeProjectDir
  where
    wasiStandaloneRuntimeDir :: [(FilePath, BS.ByteString)]
    wasiStandaloneRuntimeDir = $(FE.makeRelativeToProject "c-runtime/wasi-standalone" >>= FE.embedDir)

    standaloneRuntimeDir :: [(FilePath, BS.ByteString)]
    standaloneRuntimeDir = $(FE.makeRelativeToProject "c-runtime/standalone" >>= FE.embedDir)

    wasiLibCRuntimeDir :: [(FilePath, BS.ByteString)]
    wasiLibCRuntimeDir = $(FE.makeRelativeToProject "c-runtime/wasi-libc" >>= FE.embedDir)

    builtinCRuntimeDir :: [(FilePath, BS.ByteString)]
    builtinCRuntimeDir = $(FE.makeRelativeToProject "c-runtime/builtins" >>= FE.embedDir)

    wallocDir :: [(FilePath, BS.ByteString)]
    wallocDir = $(FE.makeRelativeToProject "c-runtime/walloc" >>= FE.embedDir)

    libcRuntime :: [(FilePath, BS.ByteString)]
    libcRuntime = wasiLibCRuntimeDir <> builtinCRuntimeDir

    runtimeProjectDir :: [(FilePath, BS.ByteString)]
    runtimeProjectDir = case o ^. compileTarget of
      TargetNative -> libcRuntime
      _ -> case o ^. compileRuntime of
        RuntimeWasiStandalone -> wasiStandaloneRuntimeDir <> builtinCRuntimeDir <> wallocDir
        RuntimeWasiLibC -> libcRuntime
        RuntimeStandalone -> standaloneRuntimeDir <> builtinCRuntimeDir <> wallocDir

    writeRuntime :: (FilePath, BS.ByteString) -> IO ()
    writeRuntime (filePath, contents) =
      BS.writeFile (projRoot </> juvixBuildDir </> takeFileName filePath) contents

clangNativeCompile ::
  forall r.
  Members '[Embed IO, Error Text] r =>
  FilePath ->
  FilePath ->
  CompileOptions ->
  Sem r ()
clangNativeCompile projRoot inputFileCompile o = runClang (nativeArgs outputFile inputFile)
  where
    outputFile :: FilePath
    outputFile = maybe (takeBaseName inputFileCompile) (^. pathPath) (o ^. compileOutputFile)

    inputFile :: FilePath
    inputFile = inputCFile projRoot inputFileCompile

clangCompile ::
  forall r.
  Members '[Embed IO, App, Error Text] r =>
  Path Abs File ->
  CompileOptions ->
  Sem r ()
clangCompile inputFileCompile o = do
  root <- askRoot
  let
    clangArgs :: Sem r [String]
    clangArgs = case o ^. compileRuntime of
      RuntimeStandalone -> do
        outputFile <- getOutputFile
        standaloneLibArgs outputFile inputFile
      RuntimeWasiStandalone -> wasiStandaloneArgs outputFile inputFile
      RuntimeWasiLibC -> wasiLibcArgs outputFile inputFile

    getOutputFile :: Sem r (Path Abs File)
    getOutputFile = maybe (return defaultOutputFile) someBaseToAbs' (o ^? compileOutputFile . _Just . pathPath)

    defaultOutputFile :: Path Abs File
    defaultOutputFile = replaceExtension' ".wasm" inputFileCompile

    getInputFile :: Sem r (Path Abs File)
    getInputFile = inputCFile inputFileCompile

  clangArgs >>= runClang

sysrootEnvVar :: Members '[Embed IO] r => Sem r (Path Abs Dir)
sysrootEnvVar = absDir <$>
  fromMaybeM (throw msg) (embed (lookupEnv "WASI_SYSROOT_PATH"))
  where
    msg :: Text
    msg = "Missing environment variable WASI_SYSROOT_PATH"

commonArgs :: Path Abs File -> [String]
commonArgs wasmOutputFile =
  [ "-std=c99",
    "-Oz",
    "-I",
    juvixBuildDir,
    "-o",
    toFilePath wasmOutputFile
  ]

standaloneLibArgs :: Members '[App, Embed IO] r => Path Abs File -> Path Abs File -> Sem r [String]
standaloneLibArgs wasmOutputFile inputFile = do
  root <- askRoot
  return $ commonArgs wasmOutputFile
    <> [ "--target=wasm32",
         "-nodefaultlibs",
         "-nostartfiles",
         "-Wl,--no-entry",
         toFilePath (projRoot <//> juvixBuildDir' <//> $(mkRelFile "walloc.c")),
         toFilePath inputFile
       ]

wasiStandaloneArgs :: Members '[App, Embed IO] r => Path Abs File -> Path Abs File -> Sem r [String]
wasiStandaloneArgs wasmOutputFile inputFile = do
  root <- askRoot
  com <- wasiCommonArgs wasmOutputFile
  return $ com
    <> [ toFilePath (root <//> juvixBuildDir' <//> $(mkRelFile "walloc.c")),
         toFilePath inputFile
       ]

wasiLibcArgs :: Members '[App, Embed IO] r => Path Abs File -> Path Abs File -> Sem r [String]
wasiLibcArgs wasmOutputFile inputFile = do
  com <- wasiCommonArgs wasmOutputFile
  return $ com <> ["-lc", toFilePath inputFile]

nativeArgs :: Path Abs File -> Path Abs File -> [String]
nativeArgs outputFile inputFile =
  commonArgs outputFile <> [toFilePath inputFile]

wasiCommonArgs :: Members '[App, Embed IO] r => Path Abs File -> Sem r [String]
wasiCommonArgs wasmOutputFile = do
  sysrootPath <- sysrootEnvVar
  return $ commonArgs wasmOutputFile
    <> [ "-nodefaultlibs",
         "--target=wasm32-wasi",
         "--sysroot",
         toFilePath sysrootPath
       ]

runClang ::
  Members '[Embed IO, Error Text] r =>
  [String] ->
  Sem r ()
runClang args = do
  (exitCode, _, err) <- embed (P.readProcessWithExitCode "clang" args "")
  case exitCode of
    ExitSuccess -> return ()
    _ -> throw (pack err)
