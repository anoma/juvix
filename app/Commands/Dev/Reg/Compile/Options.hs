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
    [ TargetWasm32Wasi,
      TargetNative64,
      TargetCasm
    ]

parseRegCompileOptions :: Parser CompileOptions
parseRegCompileOptions =
  parseCompileOptions
    regSupportedTargets
    (parseInputFile FileExtJuvixReg)
