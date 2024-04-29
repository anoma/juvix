module Commands.Dev.ImportTree.Print.Options where

import CommonOptions

newtype PrintOptions = PrintOptions
  { _printStats :: Bool
  }
  deriving stock (Data)

parsePrint :: Parser PrintOptions
parsePrint = do
  _printStats <-
    switch
      ( long "stats"
          <> help "Show some statistics"
      )
  pure PrintOptions {..}
