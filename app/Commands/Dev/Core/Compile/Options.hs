module Commands.Dev.Core.Compile.Options
  ( module Commands.Dev.Core.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Config qualified as Config

type CoreCompileOptions = CompileOptions

coreSupportedTargets :: NonEmpty CompileTarget
coreSupportedTargets =
  NonEmpty.fromList
    $ [ AppTargetNative64,
        AppTargetTree,
        AppTargetAsm,
        AppTargetReg,
        AppTargetCasm,
        AppTargetCairo
      ]
    <> [AppTargetWasm32Wasi | Config.config ^. Config.configWasm]
    <> [AppTargetRiscZeroRust | Config.config ^. Config.configRust]

parseCoreCompileOptions :: Parser CoreCompileOptions
parseCoreCompileOptions =
  parseCompileOptions
    coreSupportedTargets
    (parseInputFile FileExtJuvixAsm)
