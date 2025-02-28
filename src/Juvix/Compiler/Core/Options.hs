module Juvix.Compiler.Core.Options where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Field
import Juvix.Prelude

data CoreOptions = CoreOptions
  { _optCheckCoverage :: Bool,
    _optUnrollLimit :: Int,
    _optOptimizationLevel :: Int,
    _optInliningDepth :: Int,
    _optFieldSize :: Natural,
    _optVerify :: Bool
  }

makeLenses ''CoreOptions

defaultCoreOptions :: CoreOptions
defaultCoreOptions =
  CoreOptions
    { _optCheckCoverage = True,
      _optUnrollLimit = defaultUnrollLimit,
      _optOptimizationLevel = defaultOptimizationLevel,
      _optInliningDepth = defaultInliningDepth,
      _optFieldSize = defaultFieldSize,
      _optVerify = False
    }

fromEntryPoint :: EntryPoint -> CoreOptions
fromEntryPoint e@EntryPoint {..} =
  CoreOptions
    { _optCheckCoverage = not _entryPointNoCoverage,
      _optUnrollLimit = _entryPointUnrollLimit,
      _optOptimizationLevel = _entryPointOptimizationLevel,
      _optInliningDepth = _entryPointInliningDepth,
      _optFieldSize = _entryPointFieldSize,
      _optVerify = entryPointVerificationEnabled e
    }
