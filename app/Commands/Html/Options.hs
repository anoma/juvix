module Commands.Html.Options where

import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Backend.Html.Data.Options hiding (HtmlOptions)

data HtmlOptions = HtmlOptions
  { _htmlNonRecursive :: Bool,
    _htmlOnlySource :: Bool,
    _htmlOnlyCode :: Bool,
    _htmlTheme :: Theme,
    _htmlOutputDir :: AppPath Dir,
    _htmlInputFile :: Maybe (AppPath File),
    _htmlNoFooter :: Bool,
    _htmlFolderStructure :: Bool,
    _htmlNoPath :: Bool,
    _htmlExt :: Text,
    _htmlStripPrefix :: Text,
    _htmlAssetsPrefix :: Text,
    _htmlUrlPrefix :: Text,
    _htmlIdPrefix :: Text,
    _htmlOpen :: Bool
  }
  deriving stock (Data)

makeLenses ''HtmlOptions

parseHtml :: Parser HtmlOptions
parseHtml = do
  _htmlNonRecursive <-
    switch
      ( long "non-recursive"
          <> help "Do not process imported modules recursively"
      )
  _htmlOnlySource <-
    switch
      ( long "only-source"
          <> help "Generate only Html for the source code with syntax highlighting"
      )
  _htmlOnlyCode <-
    switch
      ( long "only-code"
          <> help "If --only-source is enabled, only generate the code without the header and footer"
      )
  _htmlTheme <-
    option
      (eitherReader parseTheme)
      ( long "theme"
          <> metavar "THEME"
          <> value LatteLight
          <> showDefault
          <> help ("Theme for syntax highlighting. " <> availableStr)
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
  _htmlNoPath <-
    switch
      ( long "no-path"
          <> help "Remove the path from all hyperlinks"
      )
  _htmlExt <-
    strOption
      ( value ".html"
          <> long "ext"
          <> showDefault
          <> help "File extension for the generated HTML files"
      )
  _htmlStripPrefix <-
    strOption
      ( value ""
          <> long "strip-prefix"
          <> showDefault
          <> help "Strip the given prefix from the hyperlinks. This has no effect if --no-path is enabled. It has precedence over --prefix-url"
      )
  _htmlFolderStructure <-
    switch
      ( long "folder-structure"
          <> help "Generate HTML following the module's folder structure"
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
  _htmlIdPrefix <-
    strOption
      ( value ""
          <> long "prefix-id"
          <> showDefault
          <> help "Prefix used for HTML element IDs"
      )
  _htmlOpen <-
    switch
      ( long "open"
          <> help "Open the documentation after generating it"
      )
  _htmlInputFile <- optional (parseInputFiles (NonEmpty.fromList [FileExtJuvix, FileExtJuvixMarkdown]))
  pure HtmlOptions {..}
  where
    allThemes :: [Theme]
    allThemes = allElements

    availableStr :: String
    availableStr =
      dotSep
        [ showCategory (light, filter ((== light) . themeLight) allThemes)
        | light <- allElements
        ]
      where
        showCategory :: (ThemeLight, [Theme]) -> String
        showCategory (light, ts) = show light <> " themes: " <> commaSep (map show ts)
        commaSep = intercalate ", "
        dotSep = intercalate ". "

    parseTheme :: String -> Either String Theme
    parseTheme s = case lookup (map toLower s) themes of
      Just t -> return t
      Nothing -> Left $ "unrecognised theme: " <> s
      where
        themes :: [(String, Theme)]
        themes = [(show theme, theme) | theme <- allThemes]
