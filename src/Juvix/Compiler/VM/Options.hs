module Juvix.Compiler.VM.Options where

import Juvix.Compiler.Defaults
import Juvix.Prelude

data Options = Options
  { _optStackSize :: Int,
    _optHeapSize :: Int,
    _optStepsNum :: Int,
    _optIntegerBits :: Int
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optStackSize = defaultStackSize,
      _optHeapSize = defaultHeapSize,
      _optStepsNum = defaultUnrollLimit,
      _optIntegerBits = defaultVampIRIntegerBits
    }
