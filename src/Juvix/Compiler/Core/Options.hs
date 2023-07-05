module Juvix.Compiler.Core.Options where

import Juvix.Compiler.Defaults
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

data CoreOptions = CoreOptions
  { _optCheckCoverage :: Bool,
    _optUnrollLimit :: Int,
    _optOptimizationLevel :: Int,
    _optInliningDepth :: Int
  }

makeLenses ''CoreOptions

defaultCoreOptions :: CoreOptions
defaultCoreOptions =
  CoreOptions
    { _optCheckCoverage = True,
      _optUnrollLimit = defaultUnrollLimit,
      _optOptimizationLevel = defaultOptimizationLevel,
      _optInliningDepth = defaultInliningDepth
    }

fromEntryPoint :: EntryPoint -> CoreOptions
fromEntryPoint EntryPoint {..} =
  CoreOptions
    { _optCheckCoverage = not _entryPointNoCoverage,
      _optUnrollLimit = _entryPointUnrollLimit,
      _optOptimizationLevel = _entryPointOptimizationLevel,
      _optInliningDepth = _entryPointInliningDepth
    }
