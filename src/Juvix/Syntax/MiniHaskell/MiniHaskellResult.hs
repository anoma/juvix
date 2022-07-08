module Juvix.Syntax.MiniHaskell.MiniHaskellResult where

import Juvix.Prelude
import Juvix.Syntax.MiniHaskell.Language
import Juvix.Syntax.MonoJuvix.MonoJuvixResult qualified as Mono

data MiniHaskellResult = MiniHaskellResult
  { _resultMonoJuvix :: Mono.MonoJuvixResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MiniHaskellResult
