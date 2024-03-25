module Commands.CompileNew.CommonOptions where

import CommonOptions
import Juvix.Compiler.Pipeline.EntryPoint

data CompileCommonOptions = CompileCommonOptions
  { _compileDebug :: Bool,
    _compileInliningDepth :: Int,
    _compileOptimizationLevel :: Maybe Int,
    _compileOutputFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''CompileCommonOptions

parseCompileCommonOptions ::
  Parser CompileCommonOptions
parseCompileCommonOptions = parseCompileCommonOptionsGeneric (parseInputFile FileExtJuvix)

parseCompileCommonOptionsGeneric ::
  Parser (AppPath File) ->
  Parser CompileCommonOptions
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
  _compileInputFile <- optional parserFile
  pure CompileCommonOptions {..}
