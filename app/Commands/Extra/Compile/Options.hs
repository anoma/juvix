module Commands.Extra.Compile.Options where

import CommonOptions hiding (show)
import Prelude (Show (show))

data CompileTarget
  = TargetWasm32Wasi
  | TargetNative64
  | TargetGeb
  deriving stock (Data, Bounded, Enum)

instance Show CompileTarget where
  show = \case
    TargetWasm32Wasi -> "wasm32-wasi"
    TargetNative64 -> "native"
    TargetGeb -> "geb"

data CompileOptions = CompileOptions
  { _compileDebug :: Bool,
    _compileCOutput :: Bool,
    _compilePreprocess :: Bool,
    _compileAssembly :: Bool,
    _compileTerm :: Bool,
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
  _compileCOutput <-
    switch
      ( short 'C'
          <> long "only-c"
          <> help "Produce C output only (for targets: wasm32-wasi, native)"
      )
  _compilePreprocess <-
    switch
      ( short 'E'
          <> long "only-preprocess"
          <> help "Run the C preprocessor only (for targets: wasm32-wasi, native)"
      )
  _compileAssembly <-
    switch
      ( short 'S'
          <> long "only-assemble"
          <> help "Produce assembly output only (for targets: wasm32-wasi, native)"
      )
  _compileTerm <-
    switch
      ( short 'T'
          <> long "only-term"
          <> help "Produce term output only (for targets: geb)"
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
