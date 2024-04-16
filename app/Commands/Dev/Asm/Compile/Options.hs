module Commands.Dev.Asm.Compile.Options
  ( module Commands.Dev.Asm.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions

type AsmCompileOptions = CompileOptions

asmSupportedTargets :: NonEmpty CompileTarget
asmSupportedTargets =
  AppTargetWasm32Wasi
    :| [ AppTargetNative64,
         AppTargetReg,
         AppTargetCasm,
         AppTargetCairo
       ]

parseAsmCompileOptions :: Parser AsmCompileOptions
parseAsmCompileOptions =
  parseCompileOptions
    asmSupportedTargets
    (parseInputFile FileExtJuvixAsm)
