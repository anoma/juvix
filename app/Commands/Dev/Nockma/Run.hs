module Commands.Dev.Nockma.Run where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Run.Options
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.EvalCompiled
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members AppEffects r) => NockmaRunOptions -> Sem r ()
runCommand opts = do
  afile <- fromAppPathFile inputFile
  argsFile <- mapM fromAppPathFile (opts ^. nockmaRunArgs)
  parsedArgs <- runAppError @JuvixError (mapM Nockma.cueJammedFileOrPretty argsFile)
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty afile)
  case parsedTerm of
    TermAtom {} -> exitFailMsg "Expected nockma input to be a cell"
    t@(TermCell {})
      | opts ^. nockmaRunAnoma -> do
          anomaDir <- getAnomaPath
          runInAnoma anomaDir t (unfoldTuple parsedArgs)
      | otherwise -> do
          let formula = anomaCallTuple parsedArgs
          (counts, res) <-
            runOpCounts
              . runReader defaultEvalOptions
              . runOutputSem @(Term Natural) (logInfo . mkAnsiText . ppTrace)
              $ evalCompiledNock' t formula
          putStrLn (ppPrint res)
          let statsFile = replaceExtension' ".profile" afile
          writeFileEnsureLn statsFile (prettyText counts)
  where
    getAnomaPath :: Sem r AnomaPath
    getAnomaPath = do
      apppath <- maybe err return (opts ^. nockmaRunAnomaDir)
      AnomaPath <$> fromAppPathDir apppath
      where
        err :: Sem r x
        err = exitFailMsg ("The --" <> anomaDirOptLongStr <> " must be provided")

    inputFile :: AppPath File
    inputFile = opts ^. nockmaRunFile

runInAnoma :: (Members AppEffects r) => AnomaPath -> Term Natural -> [Term Natural] -> Sem r ()
runInAnoma anoma t args = runAnoma anoma $ do
  res <- runAppError @SimpleError (runNockma t args)
  putStrLn (ppPrint res)
