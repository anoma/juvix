module Commands.Dev.Tree.Compile.Options
  ( module Commands.Dev.Tree.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

treeSupportedTargets :: NonEmpty CompileTarget
treeSupportedTargets =
  NonEmpty.fromList
    [ TargetWasm32Wasi,
      TargetNative64,
      TargetAsm
    ]

parseTreeCompileOptions :: Parser CompileOptions
parseTreeCompileOptions =
  parseCompileOptions
    treeSupportedTargets
    (parseInputFile FileExtJuvixTree)
