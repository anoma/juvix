module Commands.Dev.Casm.Options where

import Commands.Dev.Casm.Read.Options
import Commands.Dev.Casm.Run.Options
import CommonOptions

data CasmCommand
  = Run CasmRunOptions
  | Read CasmReadOptions
  deriving stock (Data)

parseCasmCommand :: Parser CasmCommand
parseCasmCommand =
  hsubparser $
    mconcat
      [ commandRun,
        commandRead
      ]
  where
    commandRun :: Mod CommandFields CasmCommand
    commandRun = command "run" runInfo

    commandRead :: Mod CommandFields CasmCommand
    commandRead = command "read" readInfo

    runInfo :: ParserInfo CasmCommand
    runInfo =
      info
        (Run <$> parseCasmRunOptions)
        (progDesc "Run a CASM file and print the result")

    readInfo :: ParserInfo CasmCommand
    readInfo =
      info
        (Read <$> parseCasmReadOptions)
        (progDesc "Parse a CASM file and pretty print it")
