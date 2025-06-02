module Commands.Dev.ImportTree.ScanFile.Options where

import CommonOptions
import Juvix.Compiler.Concrete.Translation.ImportScanner

data ScanFileOptions = ScanFileOptions
  { _scanFileFile :: AppPath File,
    _scanFilePrintLoc :: Bool,
    _scanFileCheck :: Bool,
    _scanFileStrategy :: ImportScanStrategy
  }
  deriving stock (Data)

makeLenses ''ScanFileOptions

parseScanFile :: Parser ScanFileOptions
parseScanFile = do
  _scanFileFile <- parseInputFiles (FileExtJuvix :| [FileExtJuvixMarkdown])
  _scanFileStrategy <- optImportScanStrategy
  _scanFilePrintLoc <-
    switch
      ( long "print-loc"
          <> help "Print the location of each import"
      )
  _scanFileCheck <-
    switch
      ( long "check"
          <> help "Checks that the rest of the backends coincide"
      )
  pure ScanFileOptions {..}
