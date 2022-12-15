module Commands.Html.Options where

import CommonOptions
import Juvix.Compiler.Backend.Html.Data.Theme

data HtmlOptions = HtmlOptions
  { _htmlRecursive :: Bool,
    _htmlTheme :: Theme,
    _htmlOutputDir :: AppPath Dir,
    _htmlInputFile :: AppPath File,
    _htmlPrintMetadata :: Bool
  }
  deriving stock (Data)

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
          <> completeWith (map show allThemes)
      )
  _htmlOutputDir <-
    parseGenericOutputDir
      ( value (Rel $(mkRelDir "html"))
          <> showDefault
          <> help "html output directory"
          <> action "directory"
      )
  _htmlPrintMetadata <-
    switch
      ( long "print-metadata"
          <> help "Add HTML footer with metadata"
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
