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
  nockmaFile :: Path Abs File <- getOutputFile FileExtNockma inputFile moutputFile
  res <- compileAnomaOpts opts'
  outputAnomaResult (opts' ^. compileDebug) nockmaFile res

compileAnoma :: (Members AppEffects r) => Maybe (AppPath File) -> Sem r Nockma.AnomaResult
compileAnoma inputFile = do
  let opts' =
        defaultCompileCommonOptionsMain
          { _compileInputFile = inputFile
          }
  compileAnomaOpts opts'

compileAnomaOpts :: (Members AppEffects r) => CompileCommonOptions 'InputMain -> Sem r Nockma.AnomaResult
compileAnomaOpts opts' = do
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <-
    applyOptions opts'
      <$> getEntryPoint (opts' ^. compileInputFile)
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToAnoma
      $ coreRes
        ^. coreResultModule
  getRight r

outputAnomaResult :: (Members '[EmbedIO, App, Files] r) => Bool -> Path Abs File -> Nockma.AnomaResult -> Sem r ()
outputAnomaResult debugOutput nockmaFile Nockma.AnomaResult {..} = do
  let code = Encoding.jamToByteString _anomaClosure
      prettyNockmaFile = replaceExtensions' nockmaDebugFileExts nockmaFile
  writeFileBS nockmaFile code
  when debugOutput (writeFileEnsureLn prettyNockmaFile (Nockma.ppPrint _anomaClosure))
