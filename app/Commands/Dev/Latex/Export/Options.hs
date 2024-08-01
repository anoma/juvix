module Commands.Dev.Latex.Export.Options where

import CommonOptions

data ExportOptions = ExportOptions
  { _exportInputFile :: AppPath File,
    _exportStandalone :: Bool,
    _exportNoComments :: Bool,
    _exportFromLine :: Maybe Int,
    _exportToLine :: Maybe Int
  }
  deriving stock (Data)

makeLenses ''ExportOptions

parseExport :: Parser ExportOptions
parseExport = do
  _exportInputFile <- parseInputFiles (pure FileExtJuvix)
  _exportNoComments <-
    switch
      ( long "no-comments"
          <> help "Do not print comments"
      )
  _exportStandalone <-
    switch
      ( long "standalone"
          <> help "Output a ready to compile LaTeX file"
      )
  _exportFromLine <-
    optional $
      option
        readLineNumber
        ( long "from"
            <> metavar "LINE"
            <> help "Output from the given line onwards"
        )
  _exportToLine <-
    optional $
      option
        readLineNumber
        ( long "to"
            <> metavar "LINE"
            <> help "Output until the given line (included)"
        )
  pure ExportOptions {..}
  where
    readLineNumber :: ReadM Int
    readLineNumber = eitherReader readr
      where
        readr :: String -> Either String Int
        readr inputStr = do
          num <- readEither inputStr
          when
            (num <= 0)
            $ Left
              ( "Invalid line number "
                  <> show num
                  <> ". Line number must be at least 1"
              )
          return num
