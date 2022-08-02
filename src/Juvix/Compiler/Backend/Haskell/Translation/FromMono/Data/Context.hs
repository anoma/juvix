module Juvix.Compiler.Backend.Haskell.Translation.FromMono.Data.Context where

import Juvix.Compiler.Backend.Haskell.Language
import Juvix.Compiler.Mono.Translation.FromInternal.Data.Context qualified as Mono
import Juvix.Prelude

data Context = Context
  { _resultMonoJuvix :: Mono.MonoJuvixResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''Context
