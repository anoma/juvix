module MiniJuvix.Syntax.MiniHaskell.MiniHaskellResult where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as Micro
import MiniJuvix.Syntax.MiniHaskell.Language

data MiniHaskellResult = MiniHaskellResult
  { _resultMicroJuvixTyped :: Micro.MicroJuvixTypedResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MiniHaskellResult
