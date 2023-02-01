module Commands.Extra.Compile.Options where

import CommonOptions

data CompileTarget
  = TargetWasm32Wasi
  | TargetNative64
  | TargetC
  | TargetGeb
  deriving stock (Show, Data)

data CompileOptions = CompileOptions
  { _compileDebug :: Bool,
    _compilePreprocess :: Bool,
    _compileAssembly :: Bool,
    _compileOutputFile :: Maybe (AppPath File),
    _compileTarget :: CompileTarget,
    _compileInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''CompileOptions

parseCompileOptions :: Parser CompileOptions
parseCompileOptions = do
  _compileDebug <-
    switch
      ( short 'g'
          <> long "debug"
          <> help "Generate debug information and runtime assertions"
      )
  _compilePreprocess <-
    switch
      ( short 'E'
          <> long "preprocess"
          <> help "Run the C preprocessor only"
      )
  _compileAssembly <-
    switch
      ( short 'S'
          <> long "assemble"
          <> help "Produce assembly output"
      )
  _compileTarget <- optCompileTarget
  _compileOutputFile <- optional parseGenericOutputFile
  _compileInputFile <- parseGenericInputFile
  pure CompileOptions {..}

optCompileTarget :: Parser CompileTarget
optCompileTarget =
  option
    (eitherReader parseTarget)
    ( long "target"
        <> short 't'
        <> metavar "TARGET"
        <> value TargetNative64
        <> showDefaultWith targetShow
        <> help "select a target: wasm32-wasi, native, c, geb"
    )
  where
    parseTarget :: String -> Either String CompileTarget
    parseTarget = \case
      "wasm32-wasi" -> Right TargetWasm32Wasi
      "native" -> Right TargetNative64
      "c" -> Right TargetC
      "geb" -> Right TargetGeb
      s -> Left $ "unrecognised target: " <> s

    targetShow :: CompileTarget -> String
    targetShow = \case
      TargetWasm32Wasi -> "wasm32-wasi"
      TargetNative64 -> "native"
      TargetC -> "c"
      TargetGeb -> "geb"
