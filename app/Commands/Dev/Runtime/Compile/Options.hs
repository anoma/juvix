module Commands.Dev.Runtime.Compile.Options where

import CommonOptions

data RuntimeCompileTarget =
  TargetWasm32Wasi
   | TargetNative64
  deriving stock (Show, Data)

data RuntimeCompileOptions = RuntimeCompileOptions
  { _runtimeCompileDebug :: Bool,
    _runtimeCompilePreprocess :: Bool,
    _runtimeCompileAssembly :: Bool,
    _runtimeCompileOutputFile :: Maybe (AppPath File),
    _runtimeCompileTarget :: RuntimeCompileTarget,
    _runtimeCompileInputFile :: AppPath File
  }

makeLenses ''RuntimeCompileOptions

parseRuntimeCompileOptions :: Parser RuntimeCompileOptions
parseRuntimeCompileOptions = do
  _runtimeCompileDebug <-
    switch
      ( short 'g'
          <> long "debug"
          <> help "Generate debug information and runtime assertions"
      )
  _runtimeCompilePreprocess <-
    switch
      ( short 'E'
          <> long "preprocess"
          <> help "Run the C preprocessor only"
      )
  _runtimeCompileAssembly <-
    switch
      ( short 'S'
          <> long "assemble"
          <> help "Produce assembly output"
      )
  _runtimeCompileTarget <-
    option
      (eitherReader parseTarget)
      ( long "target"
          <> short 't'
          <> metavar "TARGET"
          <> value TargetNative64
          <> showDefaultWith targetShow
          <> help "select a target: wasm32-wasi, native"
      )
  _runtimeCompileOutputFile <- optional parseGenericOutputFile
  _runtimeCompileInputFile <- parseInputCFile
  pure RuntimeCompileOptions {..}
  where
    parseTarget :: String -> Either String RuntimeCompileTarget
    parseTarget = \case
      "wasm32-wasi" -> Right TargetWasm32Wasi
      "native" -> Right TargetNative64
      s -> Left $ "unrecognised target: " <> s

    targetShow :: RuntimeCompileTarget -> String
    targetShow = \case
      TargetWasm32Wasi -> "wasm32-wasi"
      TargetNative64 -> "native"
