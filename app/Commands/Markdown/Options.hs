module Commands.Markdown.Options where

import CommonOptions

data MarkdownOptions = MarkdownOptions
  { _markdownInputFile :: AppPath File,
    _markdownOutputDir :: AppPath Dir,
    _markdownUrlPrefix :: Text,
    _markdownIdPrefix :: Text,
    _markdownNoPath :: Bool,
    _markdownExt :: Text,
    _markdownStripPrefix :: Text,
    _markdownStdout :: Bool,
    _markdownWriteAssets :: Bool
  }
  deriving stock (Data)

makeLenses ''MarkdownOptions

parseJuvixMarkdown :: Parser MarkdownOptions
parseJuvixMarkdown = do
  _markdownUrlPrefix :: Text <-
    strOption
      ( value mempty
          <> long "prefix-url"
          <> help "Prefix used for inner Juvix hyperlinks"
      )
  _markdownIdPrefix :: Text <-
    strOption
      ( value mempty
          <> long "prefix-id"
          <> showDefault
          <> help "Prefix used for HTML element IDs"
      )
  _markdownInputFile <- parseInputFile FileExtJuvixMarkdown
  _markdownOutputDir <-
    parseGenericOutputDir
      ( value "markdown"
          <> showDefault
          <> help "Markdown output directory"
          <> action "directory"
      )
  _markdownNoPath <-
    switch
      ( long "no-path"
          <> help "Do not include the path to the input file in the HTML id hyperlinks"
      )
  _markdownExt <-
    strOption
      ( value ".html"
          <> long
            "suffix-path"
          <> showDefault
          <> help "Suffix to append to the input file path for generating the HTML hyperlinks"
      )
  _markdownStripPrefix <-
    strOption
      ( value ""
          <> long "strip-prefix"
          <> showDefault
          <> help "Strip the given prefix from the input file path for HTML hyperlinks"
      )
  _markdownWriteAssets <-
    switch
      ( long "write-assets"
          <> help "Write the CSS/JS assets to the output directory"
      )
  _markdownStdout <-
    switch
      ( long "stdout"
          <> help "Write the output to stdout instead of a file"
      )
  pure MarkdownOptions {..}
