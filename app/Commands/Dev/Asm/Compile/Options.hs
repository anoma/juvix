module Commands.Dev.Asm.Compile.Options
  ( module Commands.Dev.Asm.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Juvix.Config qualified as Config

type AsmCompileOptions = CompileOptions

asmSupportedTargets :: NonEmpty CompileTarget
asmSupportedTargets =
  AppTargetNative64
    :| [ AppTargetReg,
         AppTargetCasm,
         AppTargetCairo
       ]
    <> [AppTargetWasm32Wasi | Config.config ^. Config.configWasm]
    <> [AppTargetRiscZeroRust | Config.config ^. Config.configRust]

parseAsmCompileOptions :: Parser AsmCompileOptions
parseAsmCompileOptions =
  parseCompileOptions
    asmSupportedTargets
    (parseInputFile FileExtJuvixAsm)
