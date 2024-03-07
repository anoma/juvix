module Commands.Dev.Core.Compile where

import Commands.Base
import Commands.Dev.Core.Compile.Base
import Commands.Dev.Core.Compile.Options
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- readFile file
  tab <- getRight (mapLeft JuvixError (Core.runParserMain file defaultModuleId mempty s))
  let arg = PipelineArg opts file (Core.moduleFromInfoTable tab)
  case opts ^. compileTarget of
    TargetWasm32Wasi -> runCPipeline arg
    TargetNative64 -> runCPipeline arg
    TargetGeb -> runGebPipeline arg
    TargetVampIR -> runVampIRPipeline arg
    TargetCore -> return ()
    TargetAsm -> runAsmPipeline arg
    TargetReg -> runRegPipeline arg
    TargetTree -> runTreePipeline arg
    TargetNockma -> runNockmaPipeline arg
    TargetAnoma -> runAnomaPipeline arg
    TargetCasm -> runCasmPipeline arg
    TargetCairo -> runCairoPipeline arg
  where
    getFile :: Sem r (Path Abs File)
    getFile = getMainFile (opts ^. compileInputFile)
