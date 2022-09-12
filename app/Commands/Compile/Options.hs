module Commands.Compile.Options where

import Juvix.Prelude
import Options.Applicative
import Commands.Extra (parseInputFile)

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
    _compileOutputFile :: Maybe FilePath,
    _compileInputFile :: FilePath
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
  _compileInputFile <- parseInputFile
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
