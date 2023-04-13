module Commands.Dev.Runtime.Options where

import Commands.Dev.Runtime.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

newtype RuntimeCommand
  = Compile CompileOptions
  deriving stock (Data)

runtimeSupportedTargets :: NonEmpty CompileTarget
runtimeSupportedTargets =
  NonEmpty.fromList
    [ TargetWasm32Wasi,
      TargetNative64
    ]

parseRuntimeOptions :: Parser CompileOptions
parseRuntimeOptions =
  parseCompileOptions
    runtimeSupportedTargets
    parseInputJuvixFile

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
        (Compile <$> parseRuntimeOptions)
        (progDesc "Compile a C file with Juvix runtime included")
