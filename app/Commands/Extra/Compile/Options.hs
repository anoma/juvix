module Commands.Extra.Compile.Options where

import CommonOptions hiding (show)
import Juvix.Compiler.Defaults
import Prelude (Show (show))

data CompileTarget
  = TargetWasm32Wasi
  | TargetNative64
  | TargetGeb
  | TargetVampIR
  | TargetCore
  | TargetAsm
  | TargetVampIRVM
  deriving stock (Eq, Data, Bounded, Enum)

instance Show CompileTarget where
  show = \case
    TargetWasm32Wasi -> "wasm32-wasi"
    TargetNative64 -> "native"
    TargetGeb -> "geb"
    TargetVampIR -> "vampir"
    TargetCore -> "core"
    TargetAsm -> "asm"
    TargetVampIRVM -> "vampir-vm"

data CompileOptions = CompileOptions
  { _compileDebug :: Bool,
    _compileCOutput :: Bool,
    _compilePreprocess :: Bool,
    _compileAssembly :: Bool,
    _compileTerm :: Bool,
    _compileOutputFile :: Maybe (AppPath File),
    _compileTarget :: CompileTarget,
    _compileInputFile :: Maybe (AppPath File),
    _compileOptimizationLevel :: Maybe Int,
    _compileInliningDepth :: Int,
    _compileStackSize :: Int,
    _compileHeapSize :: Int,
    _compileStepsNum :: Int
  }
  deriving stock (Data)

makeLenses ''CompileOptions

type SupportedTargets = NonEmpty CompileTarget

allTargets :: [CompileTarget]
allTargets = allElements

parseCompileOptions ::
  SupportedTargets ->
  Parser (AppPath File) ->
  Parser CompileOptions
parseCompileOptions supportedTargets parseInputFile = do
  _compileDebug <-
    switch
      ( short 'g'
          <> long "debug"
          <> help "Generate debug information and runtime assertions. Disables optimizations"
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
    if
        | elem TargetGeb supportedTargets ->
            switch
              ( short 'G'
                  <> long "only-term"
                  <> help "Produce term output only (for targets: geb)"
              )
        | otherwise ->
            pure False
  _compileOptimizationLevel <-
    optional
      ( option
          (fromIntegral <$> naturalNumberOpt)
          ( short 'O'
              <> long "optimize"
              <> help ("Optimization level (default: " <> show defaultOptimizationLevel <> ")")
          )
      )
  _compileInliningDepth <-
    option
      (fromIntegral <$> naturalNumberOpt)
      ( long "inline"
          <> value defaultInliningDepth
          <> help ("Automatic inlining depth limit, logarithmic in the function size (default: " <> show defaultInliningDepth <> ")")
      )
  _compileStackSize <-
    if
        | elem TargetVampIRVM supportedTargets ->
            option
              (fromIntegral <$> naturalNumberOpt)
              ( long "stack"
                  <> value defaultStackSize
                  <> help "Stack size limit (for targets: vampir-vm)"
              )
        | otherwise ->
            pure 0
  _compileHeapSize <-
    if
        | elem TargetVampIRVM supportedTargets ->
            option
              (fromIntegral <$> naturalNumberOpt)
              ( long "heap"
                  <> value defaultHeapSize
                  <> help "Heap size limit (for targets: vampir-vm)"
              )
        | otherwise ->
            pure 0
  _compileStepsNum <-
    if
        | elem TargetVampIRVM supportedTargets ->
            option
              (fromIntegral <$> naturalNumberOpt)
              ( long "steps"
                  <> value defaultStepsNum
                  <> help "Steps limit (for targets: vampir-vm)"
              )
        | otherwise ->
            pure 0
  _compileTarget <- optCompileTarget supportedTargets
  _compileOutputFile <- optional parseGenericOutputFile
  _compileInputFile <- optional parseInputFile
  pure CompileOptions {..}

optCompileTarget :: SupportedTargets -> Parser CompileTarget
optCompileTarget supportedTargets =
  option
    (eitherReader parseTarget)
    ( long "target"
        <> short 't'
        <> metavar "TARGET"
        <> value TargetNative64
        <> showDefault
        <> help ("select a target: " <> show listTargets)
        <> completeWith (map show listTargets)
    )
  where
    listTargets :: [CompileTarget]
    listTargets = toList supportedTargets

    parseTarget :: String -> Either String CompileTarget
    parseTarget txt =
      maybe err return $
        lookup
          (map toLower txt)
          [(Prelude.show t, t) | t <- listTargets]
      where
        err = Left $ "unrecognised target: " <> txt
