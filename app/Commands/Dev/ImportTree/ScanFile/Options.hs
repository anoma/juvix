module Commands.Dev.ImportTree.ScanFile.Options where

import CommonOptions

data ScanFileOptions = ScanFileOptions
  { _scanFileFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''ScanFileOptions

parseScanFile :: Parser ScanFileOptions
parseScanFile = do
  _scanFileFile <- parseInputFiles (FileExtJuvix :| [FileExtJuvixMarkdown])
  pure ScanFileOptions {..}
