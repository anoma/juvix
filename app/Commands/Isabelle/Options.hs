module Commands.Isabelle.Options where

import CommonOptions

data IsabelleOptions = IsabelleOptions
  { _isabelleInputFile :: Maybe (AppPath File),
    _isabelleOutputDir :: AppPath Dir,
    _isabelleRecursive :: Bool
  }
  deriving stock (Data)

makeLenses ''IsabelleOptions

parseIsabelle :: Parser IsabelleOptions
parseIsabelle = do
  _isabelleRecursive <-
    switch
      ( long "recursive"
          <> help "Process imported modules recursively"
      )
  _isabelleOutputDir <-
    parseGenericOutputDir
      ( value "isabelle"
          <> showDefault
          <> help "Isabelle/HOL output directory"
          <> action "directory"
      )
  _isabelleInputFile <- optional (parseInputFile FileExtJuvix)
  pure IsabelleOptions {..}
