module Commands.Dev.Nockma.Run.EphemeralClient where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Run.EphemeralClient.Options
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members AppEffects r) => NockmaRunEphemeralClientOptions -> Sem r ()
runCommand opts = do
  afile <- fromAppPathFile (opts ^. nockmaRunEphemeralFile)
  argsFile <- mapM fromAppPathFile (opts ^. nockmaRunEphemeralArgs)
  parsedArgs <- runAppError @JuvixError (mapM Nockma.cueJammedFileOrPretty argsFile)
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty afile)
  case parsedTerm of
    TermAtom {} -> exitFailMsg "Expected nockma input to be a cell"
    t@(TermCell {}) -> do
      anomaDir <- AnomaPath <$> fromAppPathDir (opts ^. nockmaRunEphemeralAnomaDir)
      runInAnoma anomaDir t (maybe [] unfoldList parsedArgs)

runInAnoma :: (Members AppEffects r) => AnomaPath -> Term Natural -> [Term Natural] -> Sem r ()
runInAnoma anoma t args = runAppError @SimpleError . runAnomaEphemeral anoma $ do
  res <-
    runNockma
      RunNockmaInput
        { _runNockmaProgram = t,
          _runNockmaArgs = args
        }
  let traces = res ^. runNockmaTraces
  renderStdOutLn (annotate AnnImportant $ "Traces (" <> show (length traces) <> "):")
  forM_ traces $ \tr ->
    renderStdOutLn (ppPrint tr)
  renderStdOutLn (annotate AnnImportant "Result:")
  renderStdOutLn (ppPrint (res ^. runNockmaResult))
