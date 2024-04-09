module Commands.Dev.Tree.Compile where

import Commands.Base
import Commands.Dev.Tree.Compile.Base
import Commands.Dev.Tree.Compile.Options
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CompileOptions -> Sem r ()
runCommand opts = do
  file <- getMainFile (Just (opts ^. compileInputFile))
  s <- readFile file
  tab <- getRight (mapLeft JuvixError (Tree.runParser file s))
  let arg = PipelineArg opts file tab
  case opts ^. compileTarget of
    TargetWasm32Wasi -> runCPipeline arg
    TargetNative64 -> runCPipeline arg
    TargetGeb -> return ()
    TargetVampIR -> return ()
    TargetCore -> return ()
    TargetAsm -> runAsmPipeline arg
    TargetReg -> runRegPipeline arg
    TargetTree -> return ()
    TargetAnoma -> runAnomaPipeline arg
    TargetCasm -> runCasmPipeline arg
    TargetCairo -> runCairoPipeline arg
