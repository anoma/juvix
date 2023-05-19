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
  let arg = PipelineArg opts file tab
  case opts ^. compileTarget of
    TargetWasm32Wasi -> runCPipeline arg
    TargetNative64 -> runCPipeline arg
    TargetGeb -> runGebPipeline arg
    TargetVampIR -> runVampIRPipeline arg
    TargetCore -> return ()
    TargetAsm -> runAsmPipeline arg
  where
    getFile :: Sem r (Path Abs File)
    getFile = fromAppPathFile (opts ^. compileInputFile)
