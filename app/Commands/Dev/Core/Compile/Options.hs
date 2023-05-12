module Commands.Dev.Core.Compile.Options
  ( module Commands.Dev.Core.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

type CoreCompileOptions = CompileOptions

coreSupportedTargets :: NonEmpty CompileTarget
coreSupportedTargets =
  NonEmpty.fromList
    [ TargetWasm32Wasi,
      TargetNative64,
      TargetGeb,
      TargetVampIR,
      TargetAsm
    ]

parseCoreCompileOptions :: Parser CoreCompileOptions
parseCoreCompileOptions =
  parseCompileOptions
    coreSupportedTargets
    parseInputJuvixAsmFile
