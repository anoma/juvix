module Commands.Dev.Reg.Compile.Options
  ( module Commands.Dev.Reg.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Juvix.Config qualified as Config

regSupportedTargets :: NonEmpty CompileTarget
regSupportedTargets =
  AppTargetNative64
    :| [ AppTargetCasm,
         AppTargetCairo
       ]
    <> [AppTargetWasm32Wasi | Config.config ^. Config.configWasm]
    <> [AppTargetRiscZeroRust | Config.config ^. Config.configRust]

parseRegCompileOptions :: Parser CompileOptions
parseRegCompileOptions =
  parseCompileOptions
    regSupportedTargets
    (parseInputFile FileExtJuvixReg)
