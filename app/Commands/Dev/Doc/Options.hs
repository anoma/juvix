module Commands.Dev.Doc.Options where

import CommonOptions

data DocOptions = DocOptions
  { _docOutputDir :: Path,
    _docOpen :: Bool,
    _docInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''DocOptions

parseDoc :: Parser DocOptions
parseDoc = do
  _docOutputDir <-
    parseGenericOutputDir
      ( value "doc"
          <> showDefault
          <> help "html output directory"
      )
  _docOpen <-
    switch
      ( long "open"
          <> help "open the documentation after generating it"
      )
  _docInputFile <- parseInputJuvixFile
  pure DocOptions {..}
