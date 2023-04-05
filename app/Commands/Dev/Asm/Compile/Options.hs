module Commands.Dev.Asm.Compile.Options
  ( module Commands.Dev.Asm.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

type AsmCompileOptions = CompileOptions

asmSupportedTargets :: NonEmpty CompileTarget
asmSupportedTargets =
  NonEmpty.fromList
    ( [ TargetWasm32Wasi,
        TargetNative64,
        TargetAsm
      ]
    )

parseAsmCompileOptions :: Parser AsmCompileOptions
parseAsmCompileOptions =
  parseCompileOptions
    asmSupportedTargets
    parseInputJuvixAsmFile
