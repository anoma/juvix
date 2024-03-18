module Commands.Dev.Nockma.Run where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Run.Options
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.EvalCompiled
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma
import Juvix.Parser.Error

runCommand :: forall r. (Members '[EmbedIO, App] r) => NockmaRunOptions -> Sem r ()
runCommand opts = do
  afile <- fromAppPathFile inputFile
  envfile <- fromAppPathFile (opts ^. nockmaRunEnvFile)
  argsFile <- mapM fromAppPathFile (opts ^. nockmaRunArgs)
  parsedEnvTerm <- Nockma.parseTermFile envfile >>= checkParsed
  parsedArgs <- mapM (Nockma.parseTermFile >=> checkParsed) argsFile
  parsedTerm <- Nockma.parseTermFile afile >>= checkParsed
  case parsedTerm of
    t@(TermCell {}) -> do
      let formula = anomaCallTuple parsedEnvTerm parsedArgs
      res <-
          runReader defaultEvalOptions
          . runOutputSem @(Term Natural) (say . ppTrace)
          $ evalCompiledNock' t formula
      putStrLn (ppPrint res)
    TermAtom {} -> exitFailMsg "Expected nockma input to be a cell"
  where
    inputFile :: AppPath File
    inputFile = opts ^. nockmaRunFile

    checkParsed :: Either MegaparsecError (Term Natural) -> Sem r (Term Natural)
    checkParsed = \case
      Left err -> exitJuvixError (JuvixError err)
      Right tm -> return tm
