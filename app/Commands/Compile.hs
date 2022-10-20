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

inputCFile :: FilePath -> FilePath -> FilePath
inputCFile projRoot inputFileCompile =
  projRoot </> juvixBuildDir </> outputMiniCFile
  where
    outputMiniCFile :: FilePath
    outputMiniCFile = takeBaseName inputFileCompile <> ".c"

runCompile :: FilePath -> FilePath -> CompileOptions -> Text -> IO (Either Text ())
runCompile projRoot inputFileCompile o minic = do
  createDirectoryIfMissing True (projRoot </> juvixBuildDir)
  TIO.writeFile (inputCFile projRoot inputFileCompile) minic
  prepareRuntime projRoot o
  case o ^. compileTarget of
    TargetWasm -> runM (runError (clangCompile projRoot inputFileCompile o))
    TargetC -> return (Right ())
    TargetNative -> runM (runError (clangNativeCompile projRoot inputFileCompile o))

prepareRuntime :: FilePath -> CompileOptions -> IO ()
prepareRuntime projRoot o = do
  mapM_ writeRuntime runtimeProjectDir
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
  Members '[Embed IO, Error Text] r =>
  FilePath ->
  FilePath ->
  CompileOptions ->
  Sem r ()
clangCompile projRoot inputFileCompile o = clangArgs >>= runClang
  where
    clangArgs :: Sem r [String]
    clangArgs = case o ^. compileRuntime of
      RuntimeStandalone ->
        return (standaloneLibArgs projRoot outputFile inputFile)
      RuntimeWasiStandalone -> wasiStandaloneArgs projRoot outputFile inputFile <$> sysrootEnvVar
      RuntimeWasiLibC -> wasiLibcArgs outputFile inputFile <$> sysrootEnvVar

    outputFile :: FilePath
    outputFile = maybe (takeBaseName inputFileCompile <> ".wasm") (^. pathPath) (o ^. compileOutputFile)

    inputFile :: FilePath
    inputFile = inputCFile projRoot inputFileCompile

    sysrootEnvVar :: Sem r String
    sysrootEnvVar =
      fromMaybeM (throw msg) (embed (lookupEnv "WASI_SYSROOT_PATH"))
      where
        msg :: Text
        msg = "Missing environment variable WASI_SYSROOT_PATH"

commonArgs :: FilePath -> [String]
commonArgs wasmOutputFile =
  [ "-std=c99",
    "-Oz",
    "-I",
    juvixBuildDir,
    "-o",
    wasmOutputFile
  ]

standaloneLibArgs :: FilePath -> FilePath -> FilePath -> [String]
standaloneLibArgs projRoot wasmOutputFile inputFile =
  commonArgs wasmOutputFile
    <> [ "--target=wasm32",
         "-nodefaultlibs",
         "-nostartfiles",
         "-Wl,--no-entry",
         projRoot </> juvixBuildDir </> "walloc.c",
         inputFile
       ]

wasiStandaloneArgs :: FilePath -> FilePath -> FilePath -> FilePath -> [String]
wasiStandaloneArgs projRoot wasmOutputFile inputFile sysrootPath =
  wasiCommonArgs sysrootPath wasmOutputFile
    <> [ projRoot </> juvixBuildDir </> "walloc.c",
         inputFile
       ]

wasiLibcArgs :: FilePath -> FilePath -> FilePath -> [String]
wasiLibcArgs wasmOutputFile inputFile sysrootPath =
  wasiCommonArgs sysrootPath wasmOutputFile
    <> ["-lc", inputFile]

nativeArgs :: FilePath -> FilePath -> [String]
nativeArgs outputFile inputFile =
  commonArgs outputFile <> [inputFile]

wasiCommonArgs :: FilePath -> FilePath -> [String]
wasiCommonArgs sysrootPath wasmOutputFile =
  commonArgs wasmOutputFile
    <> [ "-nodefaultlibs",
         "--target=wasm32-wasi",
         "--sysroot",
         sysrootPath
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
