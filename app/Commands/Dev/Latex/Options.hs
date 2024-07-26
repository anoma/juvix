module Commands.Dev.Latex.Options where

import CommonOptions

newtype LatexOptions = LatexOptions
  { _latexInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''LatexOptions

parseLatex :: Parser LatexOptions
parseLatex = do
  _latexInputFile <- parseInputFiles (pure FileExtJuvix)
  pure LatexOptions {..}
