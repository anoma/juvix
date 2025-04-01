module Commands.Compile.Anoma where

import Commands.Base
import Commands.Compile.Anoma.Options
import Commands.Extra.NewCompile
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Nockma.Data.Module qualified as Nockma
import Juvix.Compiler.Nockma.Encoding.Jam qualified as Encoding
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Pipeline.Modular

runCommand :: (Members AppEffects r) => AnomaOptions 'InputMain -> Sem r ()
runCommand opts@AnomaOptions {..} = do
  let copts = opts ^. anomaCompileCommonOptions
      inputFile = copts ^. compileInputFile
      moutputFile = copts ^. compileOutputFile
  nockmaFile :: Path Abs File <- getOutputFile FileExtNockma inputFile moutputFile
  copts' <- fromCompileCommonOptionsMain _anomaCompileCommonOptions
  let opts' = AnomaOptions {_anomaCompileCommonOptions = copts', ..}
  if
      | _anomaModular -> do
          (mid, mtab) <- runPipelineModular opts' (Just (copts' ^. compileInputFile)) Nothing modularCoreToAnoma
          outputAnomaModuleTable (copts' ^. compileDebug) nockmaFile mid mtab
      | otherwise -> do
          r <- runError @JuvixError $ runPipeline opts' (_anomaCompileCommonOptions ^. compileInputFile) upToAnoma
          res <- getRight r
          outputAnomaModule (copts' ^. compileDebug) nockmaFile res

outputAnomaModule :: (Members '[EmbedIO, App, Files] r) => Bool -> Path Abs File -> Nockma.Module -> Sem r ()
outputAnomaModule debugOutput nockmaFile Nockma.Module {..} = do
  let code = fromJust (_moduleInfoTable ^. Nockma.infoCode)
      code' = Encoding.jamToByteString code
      prettyNockmaFile = replaceExtensions' nockmaDebugFileExts nockmaFile
  writeFileBS nockmaFile code'
  when debugOutput (writeFileEnsureLn prettyNockmaFile (Nockma.ppPrint code))

outputAnomaModuleTable :: (Members '[EmbedIO, App, Files] r) => Bool -> Path Abs File -> ModuleId -> Nockma.ModuleTable -> Sem r ()
outputAnomaModuleTable debugOutput nockmaFile mid mtab = do
  let md = Nockma.lookupModuleTable mtab mid
  outputAnomaModule debugOutput nockmaFile md
  let storageNockmaFile = replaceExtensions' nockmaStorageFileExts nockmaFile
      modules =
        Encoding.jamToByteString
          . Nockma.makeList
          . map Nockma.getModuleCode
          . HashMap.elems
          $ mtab ^. Nockma.moduleTable
  writeFileBS storageNockmaFile modules
