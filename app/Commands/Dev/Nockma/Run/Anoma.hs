module Commands.Dev.Nockma.Run.Anoma where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

data RunCommandArgs = RunCommandArgs
  { _runCommandArgsFile :: Maybe (AppPath File),
    _runCommandProgramFile :: AppPath File
  }

makeLenses ''RunCommandArgs

runInAnoma :: forall r. (Members '[Error SimpleError, Anoma] r, Members AppEffects r) => RunCommandArgs -> Sem r ()
runInAnoma runArgs = do
  afile <- fromAppPathFile (runArgs ^. runCommandProgramFile)
  argsFile <- mapM fromAppPathFile (runArgs ^. runCommandArgsFile)
  parsedArgs <- runAppError @JuvixError (mapM Nockma.cueJammedFileOrPretty argsFile)
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty afile)
  case parsedTerm of
    TermAtom {} -> exitFailMsg "Expected nockma input to be a cell"
    t@(TermCell {}) -> go t (maybe [] unfoldList parsedArgs)
  where
    go :: Term Natural -> [Term Natural] -> Sem r ()
    go t args = do
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
