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
    [ TargetWasm32Wasi,
      TargetNative64,
      TargetAsm,
      TargetReg,
      TargetNockma
    ]

parseTreeCompileOptions :: Parser CompileOptions
parseTreeCompileOptions =
  parseCompileOptions
    treeSupportedTargets
    (parseInputFile FileExtJuvixTree)
