module Juvix.Compiler.Core.Options where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

data CoreOptions = CoreOptions
  { _optCheckCoverage :: Bool,
    _optUnrollLimit :: Int
  }

makeLenses ''CoreOptions

defaultCoreOptions :: CoreOptions
defaultCoreOptions =
  CoreOptions
    { _optCheckCoverage = True,
      _optUnrollLimit = 140
    }

fromEntryPoint :: EntryPoint -> CoreOptions
fromEntryPoint EntryPoint {..} =
  CoreOptions
    { _optCheckCoverage = not _entryPointNoCoverage,
      _optUnrollLimit = _entryPointUnrollLimit
    }
