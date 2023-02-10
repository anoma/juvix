module Commands.Dev.Core.Compile where

import Commands.Base
import Commands.Dev.Core.Compile.Base
import Commands.Dev.Core.Compile.Options
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. (Members '[Embed IO, App] r) => CompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- embed (readFile (toFilePath file))
  tab <- getRight (mapLeft JuvixError (Core.runParserMain file Core.emptyInfoTable s))
  case opts ^. compileTarget of
    TargetGeb -> runGebPipeline (PipelineArg opts file tab)
    TargetWasm32Wasi -> runCPipeline (PipelineArg opts file tab)
    TargetNative64 -> runCPipeline (PipelineArg opts file tab)
  where
    getFile :: Sem r (Path Abs File)
    getFile = someBaseToAbs' (opts ^. compileInputFile . pathPath)
