{-# LANGUAGE ApplicativeDo #-}

module Commands.Parse where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

data ParseOptions = ParseOptions
  { _parseInputFile :: FilePath,
    _parseNoPrettyShow :: Bool
  }

makeLenses ''ParseOptions

parseParse :: Parser ParseOptions
parseParse = do
  _parseInputFile <- parserInputFile
  _parseNoPrettyShow <-
    switch
      ( long "no-pretty-show"
          <> help "Disable formatting of the Haskell AST"
      )
  pure ParseOptions {..}
