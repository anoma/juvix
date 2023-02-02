module Commands.Extra.Compile.Options where

import CommonOptions hiding (show)
import Prelude (Show (show))

data CompileTarget
  = TargetWasm32Wasi
  | TargetNative64
  | TargetC
  | TargetGeb
  deriving stock (Data, Bounded, Enum)

instance Show CompileTarget where
  show = \case
    TargetWasm32Wasi -> "wasm32-wasi"
    TargetNative64 -> "native"
    TargetC -> "c"
    TargetGeb -> "geb"

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
        <> showDefault
        <> help ("select a target: " <> show allTargets)
        <> completeWith (map show allTargets)
    )
  where
    allTargets :: [CompileTarget]
    allTargets = allElements
    parseTarget :: String -> Either String CompileTarget
    parseTarget txt = maybe err return (lookup (map toLower txt) [(Prelude.show t, t) | t <- allElements])
      where
        err = Left $ "unrecognised target: " <> txt
