module Commands.Html.Options where

import CommonOptions
import Juvix.Compiler.Backend.Html.Data.Theme

data HtmlOptions = HtmlOptions
  { _htmlRecursive :: Bool,
    _htmlPlain :: Bool,
    _htmlTheme :: Theme,
    _htmlOutputDir :: AppPath Dir,
    _htmlInputFile :: AppPath File,
    _htmlPrintMetadata :: Bool,
    _htmlBaseUrl :: Text,
    _htmlOpen :: Bool
  }
  deriving stock (Data)

makeLenses ''HtmlOptions

parseHtml :: Parser HtmlOptions
parseHtml = do
  _htmlRecursive <-
    switch
      ( long "recursive"
          <> help "Export imported modules recursively"
      )
  _htmlPlain <-
    switch
      ( long "plain"
          <> help "Generate a Html file with the highlighted source code"
      )
  _htmlTheme <-
    option
      (eitherReader parseTheme)
      ( long "theme"
          <> metavar "THEME"
          <> value Ayu
          <> showDefault
          <> help "Color theme for when the flag --only-source is used. Options: ayu (light) and nord (dark)."
          <> completeWith (map show allThemes)
      )
  _htmlOutputDir <-
    parseGenericOutputDir
      ( value (Rel $(mkRelDir "html"))
          <> showDefault
          <> help "Html output directory"
          <> action "directory"
      )
  _htmlPrintMetadata <-
    switch
      ( long "print-metadata"
          <> help "Add HTML footer with metadata"
      )
  _htmlBaseUrl <-
    strOption
      ( value ""
          <> long "base-dir"
          <> showDefault
          <> help "Prefix for hyperlinks and assets urls"
      )
  _htmlOpen <-
    switch
      ( long "open"
          <> help "Open the documentation after generating it"
      )
  _htmlInputFile <- parseInputJuvixFile
  pure HtmlOptions {..}
  where
    allThemes :: [Theme]
    allThemes = allElements

    parseTheme :: String -> Either String Theme
    parseTheme s = case map toLower s of
      "nord" -> Right Nord
      "ayu" -> Right Ayu
      _ -> Left $ "unrecognised theme: " <> s
