module Juvix.Compiler.Store.Options where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Serialize
import Juvix.Prelude

data Options = Options
  { _optionsNoTermination :: Bool,
    _optionsNoPositivity :: Bool,
    _optionsNoCoverage :: Bool,
    _optionsNoStdlib :: Bool,
    _optionsDebug :: Bool,
    _optionsUnsafe :: Bool,
    _optionsUnrollLimit :: Int,
    _optionsOptimizationLevel :: Int,
    _optionsInliningDepth :: Int
  }
  deriving stock (Show, Eq, Generic)

instance Serialize Options

makeLenses ''Options

fromEntryPoint :: EntryPoint -> Options
fromEntryPoint EntryPoint {..} =
  Options
    { _optionsNoTermination = _entryPointNoTermination,
      _optionsNoPositivity = _entryPointNoPositivity,
      _optionsNoCoverage = _entryPointNoCoverage,
      _optionsNoStdlib = _entryPointNoStdlib,
      _optionsDebug = _entryPointDebug,
      _optionsUnsafe = _entryPointUnsafe,
      _optionsUnrollLimit = _entryPointUnrollLimit,
      _optionsOptimizationLevel = _entryPointOptimizationLevel,
      _optionsInliningDepth = _entryPointInliningDepth
    }
