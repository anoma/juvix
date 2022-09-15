module Commands.Dev.Internal.CoreEval.Options where

import CommonOptions

data InternalCoreEvalOptions = InternalCoreEvalOptions
  { _internalCoreEvalShowDeBruijn :: Bool,
    _internalCoreEvalNoIO :: Bool,
    _internalCoreEvalInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''InternalCoreEvalOptions

parseInternalCoreEval :: Parser InternalCoreEvalOptions
parseInternalCoreEval = do
  _internalCoreEvalShowDeBruijn <-
    switch
      ( long "show-de-bruijn"
          <> help "Show variable de Bruijn indices"
      )
  _internalCoreEvalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  _internalCoreEvalInputFile <- parseInputJuvixFile
  pure InternalCoreEvalOptions {..}
