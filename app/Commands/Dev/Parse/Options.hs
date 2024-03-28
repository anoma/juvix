module Commands.Dev.Parse.Options where

import CommonOptions

data ParseOptions = ParseOptions
  { _parseOptionsNoPrettyShow :: Bool,
    _parseOptionsInputFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''ParseOptions

parseParse :: Parser ParseOptions
parseParse = do
  _parseOptionsNoPrettyShow <-
    switch
      ( long "no-pretty-show"
          <> help "Disable formatting of the Haskell AST"
      )
  _parseOptionsInputFile <- optional (parseInputFile FileExtJuvix)
  pure ParseOptions {..}
