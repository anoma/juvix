module Commands.Lean.Options where

import CommonOptions
import Juvix.Compiler.Pipeline.EntryPoint

data LeanOptions = LeanOptions
  { _leanInputFile :: Maybe (AppPath File),
    _leanOutputDir :: AppPath Dir,
    _leanStdout :: Bool
  }
  deriving stock (Data)

makeLenses ''LeanOptions

parseLean :: Parser LeanOptions
parseLean = do
  _leanOutputDir <-
    parseGenericOutputDir
      ( value "lean"
          <> showDefault
          <> help "Lean output directory"
          <> action "directory"
      )
  _leanStdout <-
    switch
      ( long "stdout"
          <> help "Write the output to stdout instead of a file"
      )
  _leanInputFile <- optional (parseInputFile FileExtJuvix)
  pure LeanOptions {..}

instance EntryPointOptions LeanOptions where
  applyOptions LeanOptions {..} e = e