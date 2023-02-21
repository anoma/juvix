module Commands.Html.Options where

import CommonOptions

data HtmlOptions = HtmlOptions
  { _htmlNonRecursive :: Bool,
    _htmlOnlySource :: Bool,
    _htmlTheme :: HtmlTheme,
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
  _htmlTheme <- optTheme
  _htmlOutputDir <-
    parseGenericOutputDir
      ( value (Rel $(mkRelDir "html"))
          <> showDefault
          <> help "Html output directory"
          <> action "directory"
      )
  _htmlNoFooter <-
    switch
      ( long "no-footer"
          <> help "Remove HTML Juvix footer"
      )
  _htmlAssetsPrefix <- optAssetsPrefix
  _htmlUrlPrefix <- optUrlPrefix
  _htmlOpen <-
    switch
      ( long "open"
          <> help "Open the documentation after generating it"
      )
  _htmlInputFile <- parseInputJuvixFile
  pure HtmlOptions {..}
