module Commands.Dev.Reg.Compile.Options
  ( module Commands.Dev.Reg.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

regSupportedTargets :: NonEmpty CompileTarget
regSupportedTargets =
  NonEmpty.fromList
    [ TargetNative64,
      TargetWasm32Wasi,
      TargetCasm,
      TargetCairo
    ]

parseRegCompileOptions :: Parser CompileOptions
parseRegCompileOptions =
  parseCompileOptions
    regSupportedTargets
    (parseInputFile FileExtJuvixReg)
