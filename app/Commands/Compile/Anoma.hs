module Commands.Compile.Anoma where

import Commands.Base
import Commands.Compile.Anoma.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromTree qualified as Nockma

runCommand :: (Members AppEffects r) => AnomaOptions 'InputMain -> Sem r ()
runCommand opts = do
  let opts' = opts ^. anomaCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputFile = opts' ^. compileOutputFile
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <-
    applyOptions opts
      <$> getEntryPoint (opts' ^. compileInputFile)
  nockmaFile :: Path Abs File <- getOutputFile FileExtNockma inputFile moutputFile
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToAnoma
      $ coreRes
        ^. coreResultModule
  res <- getRight r
  outputAnomaResult nockmaFile res

outputAnomaResult :: (Members '[EmbedIO, App] r) => Path Abs File -> Nockma.AnomaResult -> Sem r ()
outputAnomaResult nockmaFile Nockma.AnomaResult {..} = do
  let code = Nockma.ppSerialize _anomaClosure
      prettyNockmaFile = replaceExtensions' [".pretty", ".nockma"] nockmaFile
  writeFileEnsureLn nockmaFile code
  writeFileEnsureLn prettyNockmaFile (Nockma.ppPrint _anomaClosure)
