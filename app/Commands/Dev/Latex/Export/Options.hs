module Commands.Dev.Latex.Export.Options where

import CommonOptions

data ExportOptions = ExportOptions
  { _exportInputFile :: AppPath File,
    _exportNoComments :: Bool
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
  pure ExportOptions {..}
