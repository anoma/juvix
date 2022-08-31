module Commands.Compile where

import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Data.Text.IO qualified as TIO
import Juvix.Prelude hiding (Doc)
import Options.Applicative
import System.Environment
import System.Process qualified as P

juvixBuildDir :: FilePath
juvixBuildDir = ".juvix-build"

data CompileTarget = TargetC | TargetWasm | TargetNative
  deriving stock (Show)

data CompileRuntime
  = RuntimeWasiStandalone
  | RuntimeWasiLibC
  | RuntimeStandalone
  deriving stock (Show)

data CompileOptions = CompileOptions
  { _compileTarget :: CompileTarget,
    _compileRuntime :: CompileRuntime,
    _compileOutputFile :: Maybe FilePath
  }

makeLenses ''CompileOptions

parseCompile :: Parser CompileOptions
parseCompile = do
  _compileTarget <-
    option
      (eitherReader parseTarget)
      ( long "target"
          <> short 't'
          <> metavar "TARGET"
          <> value TargetNative
          <> showDefaultWith targetShow
          <> help "select a target: wasm, c, native"
      )

  _compileRuntime <-
    option
      (eitherReader parseRuntime)
      ( long "runtime"
          <> short 'r'
          <> metavar "RUNTIME"
          <> value RuntimeWasiStandalone
          <> showDefaultWith runtimeShow
          <> help "select a runtime: wasi-standalone, wasi-libc, standalone"
      )

  _compileOutputFile <-
    optional $
      option
        str
        ( long "output"
            <> short 'o'
            <> metavar "OUTPUT_FILE"
            <> help "Path to output file"
            <> action "file"
        )
  pure CompileOptions {..}
  where
    parseTarget :: String -> Either String CompileTarget
    parseTarget = \case
      "wasm" -> Right TargetWasm
      "c" -> Right TargetC
      "native" -> Right TargetNative
      s -> Left $ "unrecognised target: " <> s

    targetShow :: CompileTarget -> String
    targetShow = \case
      TargetC -> "c"
      TargetWasm -> "wasm"
      TargetNative -> "native"

    parseRuntime :: String -> Either String CompileRuntime
    parseRuntime = \case
      "wasi-standalone" -> Right RuntimeWasiStandalone
      "wasi-libc" -> Right RuntimeWasiLibC
      "standalone" -> Right RuntimeStandalone
      s -> Left $ "unrecognised runtime: " <> s

    runtimeShow :: CompileRuntime -> String
    runtimeShow = \case
      RuntimeWasiStandalone -> "wasi-standalone"
      RuntimeWasiLibC -> "wasi-libc"
      RuntimeStandalone -> "standalone"

inputCFile :: FilePath -> FilePath -> FilePath
inputCFile projRoot compileInputFile =
  projRoot </> juvixBuildDir </> outputMiniCFile
  where
    outputMiniCFile :: FilePath
    outputMiniCFile = takeBaseName compileInputFile <> ".c"

runCompile :: FilePath -> FilePath -> CompileOptions -> Text -> IO (Either Text ())
runCompile projRoot compileInputFile o minic = do
  createDirectoryIfMissing True (projRoot </> juvixBuildDir)
  TIO.writeFile (inputCFile projRoot compileInputFile) minic
  prepareRuntime projRoot o
  case o ^. compileTarget of
    TargetWasm -> runM (runError (clangCompile projRoot compileInputFile o))
    TargetC -> return (Right ())
    TargetNative -> runM (runError (clangNativeCompile projRoot compileInputFile o))

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
clangNativeCompile projRoot compileInputFile o = runClang (nativeArgs outputFile inputFile)
  where
    outputFile :: FilePath
    outputFile = fromMaybe (takeBaseName compileInputFile) (o ^. compileOutputFile)

    inputFile :: FilePath
    inputFile = inputCFile projRoot compileInputFile

clangCompile ::
  forall r.
  Members '[Embed IO, Error Text] r =>
  FilePath ->
  FilePath ->
  CompileOptions ->
  Sem r ()
clangCompile projRoot compileInputFile o = clangArgs >>= runClang
  where
    clangArgs :: Sem r [String]
    clangArgs = case o ^. compileRuntime of
      RuntimeStandalone ->
        return (standaloneLibArgs projRoot outputFile inputFile)
      RuntimeWasiStandalone -> wasiStandaloneArgs projRoot outputFile inputFile <$> sysrootEnvVar
      RuntimeWasiLibC -> wasiLibcArgs outputFile inputFile <$> sysrootEnvVar

    outputFile :: FilePath
    outputFile = fromMaybe (takeBaseName compileInputFile <> ".wasm") (o ^. compileOutputFile)

    inputFile :: FilePath
    inputFile = inputCFile projRoot compileInputFile

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
