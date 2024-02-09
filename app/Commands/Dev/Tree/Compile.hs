module Commands.Dev.Tree.Compile where

import Commands.Base
import Commands.Dev.Tree.Compile.Base
import Commands.Dev.Tree.Compile.Options
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand :: forall r. (Members '[Embed IO, App, TaggedLock] r) => CompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- readFile (toFilePath file)
  tab <- getRight (mapLeft JuvixError (Tree.runParser (toFilePath file) s))
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
    TargetNockma -> runNockmaPipeline arg
  where
    getFile :: Sem r (Path Abs File)
    getFile = getMainFile (opts ^. compileInputFile)
