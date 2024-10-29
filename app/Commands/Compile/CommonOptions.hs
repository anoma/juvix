{-# LANGUAGE AllowAmbiguousTypes #-}
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

data CompileCommonOptions (k :: InputKind) = CompileCommonOptions
  { _compileInputFile :: InputFileType k,
    _compileOutputFile :: Maybe (AppPath File),
    _compileDebug :: Bool,
    _compileInliningDepth :: Int,
    _compileOptimizationLevel :: Maybe Int
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (CompileCommonOptions k)

makeLenses ''CompileCommonOptions

defaultCompileCommonOptionsMain :: CompileCommonOptions 'InputMain
defaultCompileCommonOptionsMain =
  CompileCommonOptions
    { _compileInputFile = Nothing,
      _compileOutputFile = Nothing,
      _compileDebug = False,
      _compileInliningDepth = defaultInliningDepth,
      _compileOptimizationLevel = Just defaultOptimizationLevel
    }

instance EntryPointOptions (CompileCommonOptions b) where
  applyOptions opts e =
    e
      { _entryPointDebug = opts ^. compileDebug,
        _entryPointOptimizationLevel = fromMaybe defaultOptimization (opts ^. compileOptimizationLevel),
        _entryPointInliningDepth = opts ^. compileInliningDepth
      }
    where
      defaultOptimization :: Int
      defaultOptimization
        | opts ^. compileDebug = 0
        | otherwise = defaultOptimizationLevel

fromCompileCommonOptionsMain ::
  (Members '[App] r) =>
  CompileCommonOptions 'InputMain ->
  Sem r (CompileCommonOptions ('InputExtension 'FileExtJuvix))
fromCompileCommonOptionsMain = traverseOf compileInputFile getMainAppFile

getMainFileFromInputFileType ::
  forall (k :: InputKind) r.
  (SingI k, Members '[App] r) =>
  InputFileType k ->
  Sem r (Path Abs File)
getMainFileFromInputFileType = getMainAppFileFromInputFileType @k >=> fromAppFile

getMainAppFileFromInputFileType ::
  forall (k :: InputKind) r.
  (SingI k, Members '[App] r) =>
  InputFileType k ->
  Sem r (AppPath File)
getMainAppFileFromInputFileType i = case sing :: SInputKind k of
  SInputMain -> getMainAppFile i
  SInputExtension {} -> return i

parseCompileCommonOptions' ::
  forall k.
  (SingI k) =>
  Bool ->
  Parser (CompileCommonOptions k)
parseCompileCommonOptions' allowOutputFile = do
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
  _compileOutputFile <-
    if
        | allowOutputFile ->
            optional parseGenericOutputFile
        | otherwise ->
            pure Nothing
  _compileInputFile <- parseInputFileType @k
  pure CompileCommonOptions {..}

parseCompileCommonOptions ::
  forall k.
  (SingI k) =>
  Parser (CompileCommonOptions k)
parseCompileCommonOptions = parseCompileCommonOptions' True
