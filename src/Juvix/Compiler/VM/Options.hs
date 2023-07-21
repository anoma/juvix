module Juvix.Compiler.VM.Options where

import Juvix.Compiler.Defaults
import Juvix.Prelude

data Options = Options
  { _optStackSize :: Int,
    _optHeapSize :: Int,
    _optStepsNum :: Int,
    _optIntegerBits :: Int,
    _optInputsFile :: Maybe (Path Abs File)
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optStackSize = defaultStackSize,
      _optHeapSize = defaultHeapSize,
      _optStepsNum = defaultStepsNum,
      _optIntegerBits = defaultVampIRIntegerBits,
      _optInputsFile = Nothing
    }
