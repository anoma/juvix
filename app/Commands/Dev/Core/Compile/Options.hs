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
    [ AppTargetNative64,
      AppTargetWasm32Wasi,
      AppTargetVampIR,
      AppTargetTree,
      AppTargetAsm,
      AppTargetReg,
      AppTargetCasm,
      AppTargetCairo,
      AppTargetRiscZeroRust
    ]

parseCoreCompileOptions :: Parser CoreCompileOptions
parseCoreCompileOptions =
  parseCompileOptions
    coreSupportedTargets
    (parseInputFile FileExtJuvixAsm)
