module Commands.Dev.Tree.CompileOld.Options
  ( module Commands.Dev.Tree.CompileOld.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Juvix.Config qualified as Config

treeSupportedTargets :: SupportedTargets
treeSupportedTargets =
  AppTargetNative64
    :| [ AppTargetAsm,
         AppTargetReg,
         AppTargetCasm,
         AppTargetCairo,
         AppTargetAnoma
       ]
      <> [AppTargetWasm32Wasi | Config.config ^. Config.configWasm]
      <> [AppTargetRiscZeroRust | Config.config ^. Config.configRust]

parseTreeCompileOptions :: Parser CompileOptions
parseTreeCompileOptions =
  parseCompileOptions
    treeSupportedTargets
    (parseInputFile FileExtJuvixTree)

type CompileOldOptions = CompileOptions
