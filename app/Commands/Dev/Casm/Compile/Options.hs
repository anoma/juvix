module Commands.Dev.Casm.Compile.Options
  ( module Commands.Dev.Casm.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions

casmSupportedTargets :: NonEmpty CompileTarget
casmSupportedTargets =
  AppTargetCairo
    :| []

parseCasmCompileOptions :: Parser CompileOptions
parseCasmCompileOptions =
  parseCompileOptions
    casmSupportedTargets
    (parseInputFile FileExtCasm)
