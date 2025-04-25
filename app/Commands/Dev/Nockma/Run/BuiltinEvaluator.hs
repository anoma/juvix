module Commands.Dev.Nockma.Run.BuiltinEvaluator where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Run.BuiltinEvaluator.Options
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.Encoding qualified as Encoding
import Juvix.Compiler.Nockma.EvalCompiled
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members AppEffects r) => NockmaRunBuiltinEvaluatorOptions -> Sem r ()
runCommand opts = do
  afile <- fromAppPathFile (opts ^. nockmaRunBuiltinFile)
  argsFile <- mapM fromAppPathFile (opts ^. nockmaRunBuiltinArgs)
  storageFile <- mapM fromAppPathFile (opts ^. nockmaRunBuiltinStorage)
  parsedArgs <- runAppError @JuvixError (mapM Nockma.cueJammedFileOrPretty argsFile)
  parsedStorage <- runAppError @JuvixError (mapM Nockma.cueJammedFile storageFile)
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty afile)
  case parsedTerm of
    TermAtom {} -> exitFailMsg "Expected nockma input to be a cell"
    t@(TermCell {}) -> do
      let formula = anomaCallTuple parsedArgs
          storageJammedTerms = map Encoding.atomToByteString' . mapMaybe getAtom $ maybe [] unfoldList parsedStorage
          storage = foldr insertJammedStorage emptyStorage storageJammedTerms
      (counts, res) <-
        runOpCounts
          . runReader defaultEvalOptions
          . runOutputSem @(Term Natural) (logInfo . mkAnsiText . ppTrace)
          $ evalCompiledNock storage t formula
      putStrLn (ppPrint res)
      when (opts ^. nockmaRunBuiltinProfile) $ do
        let statsFile = replaceExtension' ".profile" afile
        writeFileEnsureLn statsFile (prettyText counts)
  where
    getAtom :: Term Natural -> Maybe (Atom Natural)
    getAtom = \case
      TermAtom a -> Just a
      _ -> Nothing
