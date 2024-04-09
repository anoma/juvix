module Commands.Dev.Tree.CompileOld.Options
  ( module Commands.Dev.Tree.CompileOld.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions

treeSupportedTargets :: SupportedTargets
treeSupportedTargets =
  AppTargetNative64
    :| [ AppTargetWasm32Wasi,
         AppTargetAsm,
         AppTargetReg,
         AppTargetCasm,
         AppTargetCairo,
         AppTargetAnoma
       ]

parseTreeCompileOptions :: Parser CompileOptions
parseTreeCompileOptions =
  parseCompileOptions
    treeSupportedTargets
    (parseInputFile FileExtJuvixTree)

type CompileOldOptions = CompileOptions
