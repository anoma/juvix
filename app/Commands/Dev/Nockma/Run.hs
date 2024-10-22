module Commands.Dev.Nockma.Run where

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
  parsedTerm <- checkCued (Nockma.cueJammedFileOrPretty afile)
  case parsedTerm of
    t@(TermCell {}) -> do
      let formula = anomaCallTuple parsedArgs
      (counts, res) <-
        runOpCounts
          . runReader defaultEvalOptions
          . runOutputSem @(Term Natural) (logInfo . mkAnsiText . ppTrace)
          $ evalCompiledNock' t formula
      putStrLn (ppPrint res)
      let statsFile = replaceExtension' ".profile" afile
      writeFileEnsureLn statsFile (prettyText counts)
    TermAtom {} -> exitFailMsg "Expected nockma input to be a cell"
  where
    inputFile :: AppPath File
    inputFile = opts ^. nockmaRunFile

    checkCued :: Sem (Error JuvixError ': r) a -> Sem r a
    checkCued = runErrorNoCallStackWith exitJuvixError
