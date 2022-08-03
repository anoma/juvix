module Commands.Html where

import Juvix.Compiler.Backend.Html.Data.Theme
import Juvix.Prelude hiding (Doc)
import Options.Applicative

data HtmlOptions = HtmlOptions
  { _htmlRecursive :: Bool,
    _htmlTheme :: Theme,
    _htmlOutputDir :: FilePath,
    _htmlPrintMetadata :: Bool
  }

makeLenses ''HtmlOptions

parseHtml :: Parser HtmlOptions
parseHtml = do
  _htmlRecursive <-
    switch
      ( long "recursive"
          <> help "export imported modules recursively"
      )
  _htmlTheme <-
    option
      (eitherReader parseTheme)
      ( long "theme"
          <> metavar "THEME"
          <> value Ayu
          <> showDefault
          <> help "selects a theme: ayu (light); nord (dark)"
      )
  _htmlOutputDir <-
    option
      str
      ( long "output-dir"
          <> metavar "DIR"
          <> value "html"
          <> showDefault
          <> help "html output directory"
          <> action "directory"
      )
  _htmlPrintMetadata <-
    switch
      ( long "print-metadata"
          <> help "Add HTML footer with metadata"
      )
  pure HtmlOptions {..}
  where
    parseTheme :: String -> Either String Theme
    parseTheme s = case s of
      "nord" -> Right Nord
      "ayu" -> Right Ayu
      _ -> Left $ "unrecognised theme: " <> s
