module Commands.Compile.Options
  ( module Commands.Compile.Options,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Extra.Compile.Options
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

supportedTargets :: NonEmpty CompileTarget
supportedTargets = NonEmpty.fromList allTargets

parseMainCompileOptions :: Parser CompileOptions
parseMainCompileOptions =
  parseCompileOptions
    supportedTargets
    parseInputJuvixFile
