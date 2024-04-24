module Commands.Isabelle.Options where

import CommonOptions

data IsabelleOptions = IsabelleOptions
  { _isabelleInputFile :: Maybe (AppPath File),
    _isabelleOutputDir :: AppPath Dir,
    _isabelleStdout :: Bool
  }
  deriving stock (Data)

makeLenses ''IsabelleOptions

parseIsabelle :: Parser IsabelleOptions
parseIsabelle = do
  _isabelleOutputDir <-
    parseGenericOutputDir
      ( value "isabelle"
          <> showDefault
          <> help "Isabelle/HOL output directory"
          <> action "directory"
      )
  _isabelleStdout <-
    switch
      ( long "stdout"
          <> help "Write the output to stdout instead of a file"
      )
  _isabelleInputFile <- optional (parseInputFile FileExtJuvix)
  pure IsabelleOptions {..}
