module Commands.Dev.Reg.Compile.Options
  ( module Commands.Dev.Reg.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions

regSupportedTargets :: NonEmpty CompileTarget
regSupportedTargets =
  AppTargetNative64
    :| [ AppTargetWasm32Wasi,
         AppTargetCasm,
         AppTargetCairo
       ]

parseRegCompileOptions :: Parser CompileOptions
parseRegCompileOptions =
  parseCompileOptions
    regSupportedTargets
    (parseInputFile FileExtJuvixReg)
