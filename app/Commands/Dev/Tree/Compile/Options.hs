module Commands.Dev.Tree.Compile.Options
  ( module Commands.Dev.Tree.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions

treeSupportedTargets :: NonEmpty CompileTarget
treeSupportedTargets =
  nonEmpty'
    [ TargetNative64,
      TargetWasm32Wasi,
      TargetAsm,
      TargetReg,
      TargetCasm,
      TargetCairo,
      TargetAnoma
    ]

parseTreeCompileOptions :: Parser CompileOptions
parseTreeCompileOptions =
  parseCompileOptions
    treeSupportedTargets
    (parseInputFile FileExtJuvixTree)
