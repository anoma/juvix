module Commands.Dev.ImportTree.Print.Options where

import CommonOptions
import Juvix.Compiler.Concrete.Translation.ImportScanner

data PrintOptions = PrintOptions
  { _printStats :: Bool,
    _printScanStrategy :: ImportScanStrategy,
    _printInputFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

parsePrint :: Parser PrintOptions
parsePrint = do
  _printStats <-
    switch
      ( long "stats"
          <> help "Show some statistics"
      )
  _printScanStrategy <- optImportScanStrategy
  _printInputFile <- optional (parseInputFiles (FileExtJuvix :| [FileExtJuvixMarkdown]))
  pure PrintOptions {..}
