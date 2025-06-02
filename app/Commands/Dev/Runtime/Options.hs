module Commands.Dev.Runtime.Options where

import Commands.Dev.Runtime.Compile.Options
import CommonOptions
import Juvix.Config qualified as Config

newtype RuntimeCommand
  = Compile CompileOptions
  deriving stock (Data)

runtimeSupportedTargets :: NonEmpty CompileTarget
runtimeSupportedTargets =
  AppTargetNative64
    :| [AppTargetWasm32Wasi | Config.config ^. Config.configWasm]

parseRuntimeOptions :: Parser CompileOptions
parseRuntimeOptions =
  parseCompileOptions
    runtimeSupportedTargets
    (parseInputFile FileExtJuvix)

parseRuntimeCommand :: Parser RuntimeCommand
parseRuntimeCommand =
  hsubparser
    $ mconcat
      [ commandCompile
      ]
  where
    commandCompile :: Mod CommandFields RuntimeCommand
    commandCompile = command "compile" compileInfo

    compileInfo :: ParserInfo RuntimeCommand
    compileInfo =
      info
        (Compile <$> parseRuntimeOptions)
        (progDesc "Compile a C file with Juvix runtime included")
