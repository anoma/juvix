module Commands.Dev.Casm.Compile.Options
  ( module Commands.Dev.Casm.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

casmSupportedTargets :: NonEmpty CompileTarget
casmSupportedTargets =
  NonEmpty.fromList
    [ TargetCairo
    ]

parseCasmCompileOptions :: Parser CompileOptions
parseCasmCompileOptions =
  parseCompileOptions
    casmSupportedTargets
    (parseInputFile FileExtCasm)
