{-# LANGUAGE ApplicativeDo #-}

module Commands.Parse where

import Juvix.Prelude hiding (Doc)
import Options.Applicative

newtype ParseOptions = ParseOptions
  { _parseNoPrettyShow :: Bool
  }

makeLenses ''ParseOptions

parseParse :: Parser ParseOptions
parseParse = do
  _parseNoPrettyShow <-
    switch
      ( long "no-pretty-show"
          <> help "Disable formatting of the Haskell AST"
      )
  pure ParseOptions {..}
