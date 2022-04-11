module MiniJuvix.Syntax.MiniHaskell.MiniHaskellResult where
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MiniHaskell.Language

newtype MiniHaskellResult = MiniHaskellResult {
  _resultModules :: NonEmpty Module
                                              }

makeLenses ''MiniHaskellResult
