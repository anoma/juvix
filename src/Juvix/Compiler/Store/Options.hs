module Juvix.Compiler.Store.Options where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Serialize
import Juvix.Prelude
import Path qualified

data Options = Options
  { _optionsNoTermination :: Bool,
    _optionsNoPositivity :: Bool,
    _optionsNoCoverage :: Bool,
    _optionsNoStdlib :: Bool,
    _optionsDebug :: Bool,
    _optionsUnsafe :: Bool,
    _optionsUnrollLimit :: Int,
    _optionsOptimizationLevel :: Int,
    _optionsInliningDepth :: Int,
    _optionsFieldSize :: Natural,
    _optionsPipeline :: Maybe Pipeline,
    -- True for main file, False for imported files
    _optionsMainFile :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance Serialize Options

instance NFData Options

makeLenses ''Options

fromEntryPoint :: EntryPoint -> Options
fromEntryPoint e@EntryPoint {..} =
  Options
    { _optionsNoTermination = _entryPointNoTermination,
      _optionsNoPositivity = _entryPointNoPositivity,
      _optionsNoCoverage = _entryPointNoCoverage,
      _optionsNoStdlib = _entryPointNoStdlib,
      _optionsDebug = _entryPointDebug,
      _optionsUnsafe = _entryPointUnsafe,
      _optionsUnrollLimit = _entryPointUnrollLimit,
      _optionsOptimizationLevel = _entryPointOptimizationLevel,
      _optionsInliningDepth = _entryPointInliningDepth,
      _optionsFieldSize = _entryPointFieldSize,
      _optionsPipeline = _entryPointPipeline,
      _optionsMainFile = entryPointIsMainFile e
    }

getOptionsSubdir :: Options -> Path Rel Dir
getOptionsSubdir opts = subdir1 Path.</> subdir2
  where
    subdir1 =
      if
          | opts ^. optionsDebug -> $(mkRelDir "debug")
          | otherwise -> $(mkRelDir "release")
    subdir2 =
      case opts ^. optionsPipeline of
        Just PipelineEval -> $(mkRelDir "eval")
        Just PipelineExec -> $(mkRelDir "exec")
        Just PipelineTypecheck -> $(mkRelDir "typecheck")
        Nothing -> $(mkRelDir "default")
