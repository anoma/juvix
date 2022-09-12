module Commands.Dev.Highlight.Options
  ( module Commands.Dev.Highlight.Options,
    HighlightBackend (..),
  )
where

import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Prelude
import Options.Applicative

newtype HighlightOptions = HighlightOptions
  { _highlightBackend :: HighlightBackend
  }

parseHighlight :: Parser HighlightOptions
parseHighlight = do
  _highlightBackend <-
    option
      (eitherReader parseBackend)
      ( long "format"
          <> metavar "FORMAT"
          <> value Emacs
          <> showDefault
          <> help "selects a backend. FORMAT = emacs | json"
          <> completeWith (map show allBackends)
      )
  pure HighlightOptions {..}
  where
    allBackends :: [HighlightBackend]
    allBackends = allElements

parseBackend :: String -> Either String HighlightBackend
parseBackend s = case map toLower s of
  "emacs" -> Right Emacs
  "json" -> Right Json
  _ -> Left $ "unrecognised theme: " <> s
