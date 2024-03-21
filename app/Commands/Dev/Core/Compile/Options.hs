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
    [ TargetNative64,
      TargetWasm32Wasi,
      TargetGeb,
      TargetVampIR,
      TargetTree,
      TargetAsm,
      TargetReg,
      TargetCasm,
      TargetCairo
    ]

parseCoreCompileOptions :: Parser CoreCompileOptions
parseCoreCompileOptions =
  parseCompileOptions
    coreSupportedTargets
    (parseInputFile FileExtJuvixAsm)
