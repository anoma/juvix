module Commands.Dev.Tree.Compile where

import Commands.Base
import Commands.Dev.Tree.Compile.Anoma qualified as Anoma
import Commands.Dev.Tree.Compile.Asm qualified as Asm
import Commands.Dev.Tree.Compile.Cairo qualified as Cairo
import Commands.Dev.Tree.Compile.Casm qualified as Casm
import Commands.Dev.Tree.Compile.Native qualified as Native
import Commands.Dev.Tree.Compile.Options
import Commands.Dev.Tree.Compile.Reg qualified as Reg
import Commands.Dev.Tree.Compile.Wasi qualified as Wasi

runCommand ::
  forall r.
  (Members '[EmbedIO, App, TaggedLock] r) =>
  CompileCommand ->
  Sem r ()
runCommand = \case
  Native opts -> undefined
  Wasi opts -> Wasi.runCommand opts
  Asm opts -> Asm.runCommand opts
  Casm opts -> Casm.runCommand opts
  Reg opts -> Reg.runCommand opts
  Anoma opts -> Anoma.runCommand opts
  Cairo opts -> Cairo.runCommand opts

-- old
-- runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CompileOptions -> Sem r ()
-- runCommand opts = do
--   file <- getMainFile (Just (opts ^. compileInputFile))
--   s <- readFile file
--   tab <- getRight (Tree.runParser file s)
--   let arg = PipelineArg opts file tab
--   case opts ^. compileTarget of
--     TargetWasm32Wasi -> runCPipeline arg
--     TargetNative64 -> runCPipeline arg
--     TargetGeb -> return ()
--     TargetVampIR -> return ()
--     TargetCore -> return ()
--     TargetAsm -> runAsmPipeline arg
--     TargetReg -> runRegPipeline arg
--     TargetTree -> return ()
--     TargetAnoma -> runAnomaPipeline arg
--     TargetCasm -> runCasmPipeline arg
--     TargetCairo -> runCairoPipeline arg
