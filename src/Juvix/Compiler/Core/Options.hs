module Juvix.Compiler.Core.Options where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

newtype CoreOptions = CoreOptions
  { _optCheckCoverage :: Bool
  }

makeLenses ''CoreOptions

defaultCoreOptions :: CoreOptions
defaultCoreOptions =
  CoreOptions
    { _optCheckCoverage = True
    }

fromEntryPoint :: EntryPoint -> CoreOptions
fromEntryPoint EntryPoint {..} =
  CoreOptions
    { _optCheckCoverage = not _entryPointNoCoverage
    }
