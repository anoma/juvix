module Commands.Dev.Nockma.Eval where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Eval.Options
import Juvix.Compiler.Nockma.EvalCompiled
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members AppEffects r) => NockmaEvalOptions -> Sem r ()
runCommand opts = do
  afile <- fromAppPathFile file
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty afile)
  case parsedTerm of
    TermCell c -> do
      (counts, res) <-
        runOpCounts
          . runReader defaultEvalOptions
          . runOutputSem @(Term Natural) (logInfo . mkAnsiText . ppTrace)
          $ evalCompiledNock' (c ^. cellLeft) (c ^. cellRight)
      putStrLn (ppPrint res)
      let statsFile = replaceExtension' ".profile" afile
      writeFileEnsureLn statsFile (prettyText counts)
    TermAtom {} -> exitFailText "Expected nockma input to be a cell"
  where
    file :: AppPath File
    file = opts ^. nockmaEvalFile
