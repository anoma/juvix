module Commands.Dev.ImportTree.ScanFile.Options where

import CommonOptions
import Juvix.Compiler.Concrete.Translation.ImportScanner

data ScanFileOptions = ScanFileOptions
  { _scanFileFile :: AppPath File,
    _scanFileStrategy :: ImportScanStrategy
  }
  deriving stock (Data)

makeLenses ''ScanFileOptions

parseScanFile :: Parser ScanFileOptions
parseScanFile = do
  _scanFileFile <- parseInputFiles (FileExtJuvix :| [FileExtJuvixMarkdown])
  _scanFileStrategy <- optImportScanStrategy
  pure ScanFileOptions {..}
