module Commands.Dev.Asm.Compile.Options
  ( module Commands.Dev.Asm.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions

type AsmCompileOptions = CompileOptions

parseAsmCompileOptions :: Parser AsmCompileOptions
parseAsmCompileOptions = parseCompileOptions
