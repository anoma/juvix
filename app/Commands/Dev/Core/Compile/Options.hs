module Commands.Dev.Core.Compile.Options
  ( module Commands.Dev.Core.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions

type CoreCompileOptions = CompileOptions

parseCoreCompileOptions :: Parser CoreCompileOptions
parseCoreCompileOptions = parseCompileOptions
