{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.CommonOptions
  ( module Commands.Compile.CommonOptions,
    module Commands.Compile.CommonOptions.InputKind,
    module Juvix.Compiler.Pipeline.EntryPoint,
    module Juvix.Compiler.Backend,
  )
where

import App
import Commands.Compile.CommonOptions.InputKind
import CommonOptions
import Juvix.Compiler.Backend
import Juvix.Compiler.Pipeline.EntryPoint

-- | If the input file can be defaulted to the `main` in the `package.yaml` file, we
-- can omit the input file.
type CompileCommonOptionsMain = CompileCommonOptions 'InputMain

data CompileCommonOptions (k :: InputKind) = CompileCommonOptions
  { _compileInputFile :: InputFileType k,
    _compileOutputFile :: Maybe (AppPath File),
    _compileDebug :: Bool,
    _compileInliningDepth :: Int,
    _compileOptimizationLevel :: Maybe Int
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (CompileCommonOptions k)

makeLenses ''CompileCommonOptions

instance EntryPointOptions (CompileCommonOptions b) where
  applyOptions opts e =
    e
      { _entryPointDebug = opts ^. compileDebug,
        _entryPointOptimizationLevel = fromMaybe defaultOptimizationLevel (opts ^. compileOptimizationLevel),
        _entryPointInliningDepth = opts ^. compileInliningDepth
      }

fromCompileCommonOptionsMain ::
  (Members '[App] r) =>
  CompileCommonOptionsMain ->
  Sem r (CompileCommonOptions ('InputExtension 'FileExtJuvix))
fromCompileCommonOptionsMain = traverseOf compileInputFile getMainAppFile

parseCompileCommonOptions ::
  forall k.
  (SingI k) =>
  Parser (CompileCommonOptions k)
parseCompileCommonOptions = do
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
  _compileInputFile <- parseInputFileType @k
  pure CompileCommonOptions {..}
