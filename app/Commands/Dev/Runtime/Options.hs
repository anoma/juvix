module Commands.Dev.Runtime.Options where

import Commands.Dev.Runtime.Compile.Options
import CommonOptions

newtype RuntimeCommand
  = Compile CompileOptions
  deriving stock (Data)

parseRuntimeCommand :: Parser RuntimeCommand
parseRuntimeCommand =
  hsubparser $
    mconcat
      [ commandCompile
      ]
  where
    commandCompile :: Mod CommandFields RuntimeCommand
    commandCompile = command "compile" compileInfo

    compileInfo :: ParserInfo RuntimeCommand
    compileInfo =
      info
        (Compile <$> parseCompileOptions)
        (progDesc "Compile a C file with Juvix runtime included")
