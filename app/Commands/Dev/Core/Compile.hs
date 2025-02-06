module Commands.Dev.Core.Compile where

import Commands.Base
import Commands.Dev.Core.Compile.Base
import Commands.Dev.Core.Compile.Options
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CompileOptions -> Sem r ()
runCommand opts = do
  file <- getMainFile (Just (opts ^. compileInputFile))
  s <- readFile file
  tab <- getRight (Core.runParserMain file defaultModuleId mempty s)
  let arg =
        PipelineArg
          { _pipelineArgOptions = opts,
            _pipelineArgModule = Core.moduleFromInfoTable tab
          }
  case opts ^. compileTarget of
    AppTargetWasm32Wasi -> runCPipeline arg
    AppTargetNative64 -> runCPipeline arg
    AppTargetCore -> return ()
    AppTargetAsm -> runAsmPipeline arg
    AppTargetReg -> runRegPipeline arg
    AppTargetTree -> runTreePipeline arg
    AppTargetAnoma -> runAnomaPipeline arg
    AppTargetCasm -> runCasmPipeline arg
    AppTargetCairo -> runCairoPipeline arg
    AppTargetRiscZeroRust -> runRiscZeroRustPipeline arg
