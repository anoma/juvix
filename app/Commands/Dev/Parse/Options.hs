module Commands.Dev.Parse.Options where

import CommonOptions

data ParseOptions = ParseOptions
  { _parseNoPrettyShow :: Bool,
    _parseInputFile :: AppPath File
  }

makeLenses ''ParseOptions

parseParse :: Parser ParseOptions
parseParse = do
  _parseNoPrettyShow <-
    switch
      ( long "no-pretty-show"
          <> help "Disable formatting of the Haskell AST"
      )
  _parseInputFile <- parseInputJuvixFile
  pure ParseOptions {..}
