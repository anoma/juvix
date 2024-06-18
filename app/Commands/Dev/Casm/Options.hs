module Commands.Dev.Casm.Options where

import Commands.Dev.Casm.Compile.Options
import Commands.Dev.Casm.FromCairo.Options
import Commands.Dev.Casm.Read.Options
import Commands.Dev.Casm.Run.Options
import CommonOptions

data CasmCommand
  = Compile CompileOptions
  | Run CasmRunOptions
  | Read CasmReadOptions
  | FromCairo CasmFromCairoOptions
  deriving stock (Data)

parseCasmCommand :: Parser CasmCommand
parseCasmCommand =
  hsubparser
    $ mconcat
      [ commandCompile,
        commandRun,
        commandRead,
        commandFromCairo
      ]
  where
    commandCompile :: Mod CommandFields CasmCommand
    commandCompile = command "compile" compileInfo

    commandRun :: Mod CommandFields CasmCommand
    commandRun = command "run" runInfo

    commandRead :: Mod CommandFields CasmCommand
    commandRead = command "read" readInfo

    commandFromCairo :: Mod CommandFields CasmCommand
    commandFromCairo = command "from-cairo" fromCairoInfo

    compileInfo :: ParserInfo CasmCommand
    compileInfo =
      info
        (Compile <$> parseCasmCompileOptions)
        (progDesc "Compile a CASM file")

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

    fromCairoInfo :: ParserInfo CasmCommand
    fromCairoInfo =
      info
        (FromCairo <$> parseCasmFromCairoOptions)
        (progDesc "Disassemble Cairo bytecode into CASM")
