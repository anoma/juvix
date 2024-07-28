module Commands.Dev.Latex.Options where

import CommonOptions

data LatexOptions = LatexOptions
  { _latexInputFile :: AppPath File,
    _latexNoComments :: Bool
  }
  deriving stock (Data)

makeLenses ''LatexOptions

parseLatex :: Parser LatexOptions
parseLatex = do
  _latexInputFile <- parseInputFiles (pure FileExtJuvix)
  _latexNoComments <-
    switch
      ( long "no-comments"
          <> help "Do not print comments"
      )
  pure LatexOptions {..}
