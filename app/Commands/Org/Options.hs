module Commands.Org.Options where

import CommonOptions

data OrgOptions = OrgOptions
  { _orgTheme :: HtmlTheme,
    _orgOutputDir :: AppPath Dir,
    _orgInputFile :: AppPath File,
    _orgAssetsPrefix :: Text,
    _orgUrlPrefix :: Text,
    _orgOnlyCheck :: Bool
  }
  deriving stock (Data)

makeLenses ''OrgOptions

parseOrg :: Parser OrgOptions
parseOrg = do
  _orgInputFile <- parseGenericInputFile
  _orgTheme <- optTheme
  _orgOutputDir <-
    parseGenericOutputDir
      ( value (Rel $(mkRelDir "processed-org"))
          <> showDefault
          <> help "Org output directory"
          <> action "directory"
      )
  _orgAssetsPrefix <- optAssetsPrefix
  _orgUrlPrefix <- optUrlPrefix
  _orgOnlyCheck <-
    switch
      ( long "only-check"
          <> help "Check the source code but do not write any file"
      )
  pure OrgOptions {..}
