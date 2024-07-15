module Commands.Dev.ImportTree.Options where

import Commands.Dev.ImportTree.Print.Options
import Commands.Dev.ImportTree.ScanFile.Options
import CommonOptions

data ImportTreeCommand
  = ScanFile ScanFileOptions
  | Print PrintOptions
  deriving stock (Data)

parseImportTree :: Parser ImportTreeCommand
parseImportTree =
  hsubparser
    ( mconcat
        [ commandPrint,
          commandScanFile
        ]
    )

commandScanFile :: Mod CommandFields ImportTreeCommand
commandScanFile =
  command "scan"
    $ info
      (ScanFile <$> parseScanFile)
      (progDesc "Scan a single Juvix file and print its imports")

commandPrint :: Mod CommandFields ImportTreeCommand
commandPrint =
  command "print"
    $ info
      (Print <$> parsePrint)
      (progDesc "Print the import tree for the current package")
