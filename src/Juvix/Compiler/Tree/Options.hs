module Juvix.Compiler.Tree.Options where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Field
import Juvix.Prelude

data Options = Options
  { _optOptimizationLevel :: Int,
    _optFieldSize :: Natural
  }

makeLenses ''Options

defaultOptions :: Options
defaultOptions =
  Options
    { _optOptimizationLevel = defaultOptimizationLevel,
      _optFieldSize = defaultFieldSize
    }

fromEntryPoint :: EntryPoint -> Options
fromEntryPoint EntryPoint {..} =
  Options
    { _optOptimizationLevel = _entryPointOptimizationLevel,
      _optFieldSize = _entryPointFieldSize
    }
