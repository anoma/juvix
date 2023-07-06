module Commands.Dev.VM.Compile.Options
  ( module Commands.Dev.VM.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

type VMCompileOptions = CompileOptions

vmSupportedTargets :: NonEmpty CompileTarget
vmSupportedTargets =
  NonEmpty.fromList
    [ TargetVampIRVM
    ]

parseVMCompileOptions :: Parser VMCompileOptions
parseVMCompileOptions =
  parseCompileOptions
    vmSupportedTargets
    parseInputJuvixVMFile
