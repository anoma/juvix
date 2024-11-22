module Commands.Dev.Anoma.Prove where

import Anoma.Effect.Base
import Anoma.Effect.RunNockma
import Commands.Base
import Commands.Dev.Anoma.Base
import Commands.Dev.Anoma.Prove.Options
import Juvix.Compiler.Nockma.Encoding.Jam qualified as Encoding
import Juvix.Compiler.Nockma.Pretty hiding (Path)

runCommand :: forall r. (Members (Anoma ': Error SimpleError ': AppEffects) r) => ProveOptions -> Sem r ()
runCommand opts = do
  res <- runNock (opts ^. proveFile) (opts ^. proveArgs)
  let traces = res ^. runNockmaTraces
  forM_ traces (renderStdOutLn . ppPrint)
  let provedCode = Encoding.jamToByteString (res ^. runNockmaResult)
  outputFile <- getOutputFile (opts ^. proveOutputFile)
  writeFileBS outputFile provedCode
  where
    getOutputFile :: (Member App x) => Maybe (AppPath File) -> Sem x (Path Abs File)
    getOutputFile = \case
      Just out -> fromAppFile out
      Nothing -> do
        invokeDir <- askInvokeDir
        programFilePath <- fromAppFile (opts ^. proveFile)
        let baseOutputFile = invokeDir <//> filename programFilePath
        return (replaceExtensions' (".proved" :| [".nockma"]) baseOutputFile)
