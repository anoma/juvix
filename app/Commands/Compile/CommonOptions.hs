module Commands.Compile.CommonOptions where

import App
import CommonOptions
import Juvix.Compiler.Pipeline.EntryPoint

-- | If the input file can be defaulted to the `main` in the `package.yaml` file, we
-- can omit the input file.
type CompileCommonOptionsMain = CompileCommonOptions' (Maybe (AppPath File))

type CompileCommonOptions = CompileCommonOptions' (AppPath File)

data CompileCommonOptions' inputFile = CompileCommonOptions
  { _compileInputFile :: inputFile,
    _compileOutputFile :: Maybe (AppPath File),
    _compileDebug :: Bool,
    _compileInliningDepth :: Int,
    _compileOptimizationLevel :: Maybe Int
  }
  deriving stock (Data)

makeLenses ''CompileCommonOptions'

applyCompileCommonOptions :: CompileCommonOptions' b -> EntryPoint -> EntryPoint
applyCompileCommonOptions opts e =
  e
    { _entryPointDebug = opts ^. compileDebug,
      _entryPointOptimizationLevel = fromMaybe defaultOptimizationLevel (opts ^. compileOptimizationLevel),
      _entryPointInliningDepth = opts ^. compileInliningDepth
    }

fromCompileCommonOptionsMain :: (Members '[App] r) => CompileCommonOptionsMain -> Sem r CompileCommonOptions
fromCompileCommonOptionsMain = traverseOf compileInputFile getMainAppFile

parseCompileCommonOptionsMain ::
  Parser CompileCommonOptionsMain
parseCompileCommonOptionsMain =
  parseCompileCommonOptionsGeneric
    (optional (parseInputFile FileExtJuvix))

parseCompileCommonOptions ::
  Parser CompileCommonOptions
parseCompileCommonOptions =
  parseCompileCommonOptionsGeneric
    (parseInputFile FileExtJuvix)

parseCompileCommonOptionsGeneric ::
  Parser inputFile ->
  Parser (CompileCommonOptions' inputFile)
parseCompileCommonOptionsGeneric parserFile = do
  _compileDebug <-
    switch
      ( short 'g'
          <> long "debug"
          <> help "Generate debug information and runtime assertions. Disables optimizations"
      )
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
  _compileOutputFile <- optional parseGenericOutputFile
  _compileInputFile <- parserFile
  pure CompileCommonOptions {..}
