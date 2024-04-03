module Commands.Compile.Vampir where

import Commands.Base
import Commands.Compile.Vampir.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Backend
import Juvix.Compiler.Backend.VampIR.Translation qualified as VampIR

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => VampirOptions -> Sem r ()
runCommand opts = do
  let opts' = opts ^. vampirCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputFile = opts' ^. compileOutputFile
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <-
    set entryPointTarget (Just TargetVampIR)
      . set entryPointUnsafe (opts ^. vampirUnsafe)
      . applyCompileCommonOptions opts'
      <$> getEntryPoint (opts' ^. compileInputFile)
  vampirFile :: Path Abs File <- getOutputFile FileExtVampIR inputFile moutputFile
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToVampIR
      $ coreRes ^. coreResultModule
  VampIR.Result {..} <- getRight r
  writeFileEnsureLn vampirFile _resultCode
