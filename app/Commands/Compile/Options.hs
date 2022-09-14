module Commands.Compile.Options where

import CommonOptions

data CompileTarget = TargetC | TargetWasm | TargetNative
  deriving stock (Show, Data)

data CompileRuntime
  = RuntimeWasiStandalone
  | RuntimeWasiLibC
  | RuntimeStandalone
  deriving stock (Show, Data)

data CompileOptions = CompileOptions
  { _compileTarget :: CompileTarget,
    _compileRuntime :: CompileRuntime,
    _compileOutputFile :: Maybe Path,
    _compileInputFile :: Path
  }
  deriving stock (Data)

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
  _compileOutputFile <- optional parseGenericOutputFile
  _compileInputFile <- parseInputJuvixFile
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
