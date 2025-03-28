module Commands.Compile.Anoma where

import Commands.Base
import Commands.Compile.Anoma.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Nockma.Data.Module qualified as Nockma
import Juvix.Compiler.Nockma.Encoding.Jam qualified as Encoding
import Juvix.Compiler.Nockma.Pretty qualified as Nockma

runCommand :: (Members AppEffects r) => AnomaOptions 'InputMain -> Sem r ()
runCommand opts = do
  let opts' = opts ^. anomaCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputFile = opts' ^. compileOutputFile
  nockmaFile :: Path Abs File <- getOutputFile FileExtNockma inputFile moutputFile
  res <- compileAnomaOpts opts
  outputAnomaResult (opts' ^. compileDebug) nockmaFile res

compileAnoma :: (Members AppEffects r) => Maybe (AppPath File) -> Sem r Nockma.Module
compileAnoma inputFile = do
  let opts =
        AnomaOptions $
          defaultCompileCommonOptionsMain
            { _compileInputFile = inputFile
            }
  compileAnomaOpts opts

compileAnomaOpts :: (Members AppEffects r) => AnomaOptions 'InputMain -> Sem r Nockma.Module
compileAnomaOpts AnomaOptions {..} = do
  copts <- fromCompileCommonOptionsMain _anomaCompileCommonOptions
  let opts' = AnomaOptions {_anomaCompileCommonOptions = copts, ..}
  r <- runError @JuvixError $ runPipeline opts' (_anomaCompileCommonOptions ^. compileInputFile) upToAnoma
  getRight r

outputAnomaResult :: (Members '[EmbedIO, App, Files] r) => Bool -> Path Abs File -> Nockma.Module -> Sem r ()
outputAnomaResult debugOutput nockmaFile Nockma.Module {..} = do
  let code = fromJust (_moduleInfoTable ^. Nockma.infoCode)
      code' = Encoding.jamToByteString code
      prettyNockmaFile = replaceExtensions' nockmaDebugFileExts nockmaFile
  writeFileBS nockmaFile code'
  when debugOutput (writeFileEnsureLn prettyNockmaFile (Nockma.ppPrint code))
