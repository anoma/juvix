module Commands.Dev.Core.Eval.Options where

import CommonOptions

data CoreEvalOptions = CoreEvalOptions
  { _coreEvalNoIO :: Bool,
    _coreEvalInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''CoreEvalOptions

parseCoreEvalOptions :: Parser CoreEvalOptions
parseCoreEvalOptions = do
  _coreEvalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  _coreEvalInputFile <- parseInputJuvixFile
  pure CoreEvalOptions {..}
