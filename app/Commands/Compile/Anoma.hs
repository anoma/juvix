module Commands.Compile.Anoma where

import Commands.Base
import Commands.Compile.Anoma.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Nockma.Encoding.Jam qualified as Encoding
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
  outputAnomaResult (opts' ^. compileDebug) nockmaFile res

outputAnomaResult :: (Members '[EmbedIO, App, Files] r) => Bool -> Path Abs File -> Nockma.AnomaResult -> Sem r ()
outputAnomaResult debugOutput nockmaFile Nockma.AnomaResult {..} = do
  let code = Encoding.jamToByteString _anomaClosure
      prettyNockmaFile = replaceExtensions' [".debug", ".nockma"] nockmaFile
  writeFileBS nockmaFile code
  when debugOutput (writeFileEnsureLn prettyNockmaFile (Nockma.ppPrint _anomaClosure))
