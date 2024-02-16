module Juvix.Compiler.Core.Options where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

data CoreOptions = CoreOptions
  { _optCheckCoverage :: Bool,
    _optUnrollLimit :: Int,
    _optOptimizationLevel :: Int,
    _optInliningDepth :: Int,
    _optFieldSize :: Natural
  }

makeLenses ''CoreOptions

defaultCoreOptions :: CoreOptions
defaultCoreOptions =
  CoreOptions
    { _optCheckCoverage = True,
      _optUnrollLimit = defaultUnrollLimit,
      _optOptimizationLevel = defaultOptimizationLevel,
      _optInliningDepth = defaultInliningDepth,
      _optFieldSize = defaultFieldSize
    }

fromEntryPoint :: EntryPoint -> CoreOptions
fromEntryPoint EntryPoint {..} =
  CoreOptions
    { _optCheckCoverage = not _entryPointNoCoverage,
      _optUnrollLimit = _entryPointUnrollLimit,
      _optOptimizationLevel = _entryPointOptimizationLevel,
      _optInliningDepth = _entryPointInliningDepth,
      _optFieldSize = _entryPointFieldSize
    }
