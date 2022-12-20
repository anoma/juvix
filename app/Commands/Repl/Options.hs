module Commands.Repl.Options where

import CommonOptions

data ReplOptions = ReplOptions
  { _replInputFile :: Maybe (AppPath File),
    _replNoPrelude :: Bool
  }
  deriving stock (Data)

makeLenses ''ReplOptions

parseRepl :: Parser ReplOptions
parseRepl = do
  _replInputFile <- optional parseInputJuvixFile
  _replNoPrelude <-
    switch
      ( long "no-prelude"
          <> help "Do not load the Prelude module on launch"
      )
  pure ReplOptions {..}
