module Commands.Html.Options where

import CommonOptions
import Juvix.Compiler.Backend.Html.Data.Options hiding (HtmlOptions)

data HtmlOptions = HtmlOptions
  { _htmlNonRecursive :: Bool,
    _htmlOnlySource :: Bool,
    _htmlTheme :: Theme,
    _htmlOutputDir :: AppPath Dir,
    _htmlInputFile :: AppPath File,
    _htmlNoFooter :: Bool,
    _htmlAssetsPrefix :: Text,
    _htmlUrlPrefix :: Text,
    _htmlOpen :: Bool
  }
  deriving stock (Data)

makeLenses ''HtmlOptions

parseHtml :: Parser HtmlOptions
parseHtml = do
  _htmlNonRecursive <-
    switch
      ( long "non-recursive"
          <> help "Export imported modules recursively"
      )
  _htmlOnlySource <-
    switch
      ( long "only-source"
          <> help "Generate only Html for the source code with syntax highlighting"
      )
  _htmlTheme <-
    option
      (eitherReader parseTheme)
      ( long "theme"
          <> metavar "THEME"
          <> value Ayu
          <> showDefault
          <> help "Theme for syntax highlighting. Options: ayu (light) and nord (dark)"
          <> completeWith (map show allThemes)
      )
  _htmlOutputDir <-
    parseGenericOutputDir
      ( value "html"
          <> showDefault
          <> help "Html output directory"
          <> action "directory"
      )
  _htmlNoFooter <-
    switch
      ( long "no-footer"
          <> help "Remove HTML Juvix footer"
      )
  _htmlAssetsPrefix <-
    strOption
      ( value ""
          <> long "prefix-assets"
          <> showDefault
          <> help "Prefix used for assets's source path"
      )
  _htmlUrlPrefix <-
    strOption
      ( value ""
          <> long "prefix-url"
          <> showDefault
          <> help "Prefix used for inner Juvix hyperlinks"
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
