module Commands.Dev.Nockma.Run.Anoma where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Commands.Dev.Anoma.Base
import Juvix.Compiler.Nockma.Pretty

data RunCommandArgs = RunCommandArgs
  { _runCommandArgsFile :: Maybe (AppPath File),
    _runCommandProgramFile :: AppPath File
  }

makeLenses ''RunCommandArgs

runInAnoma :: forall r. (Members '[Error SimpleError, Anoma] r, Members AppEffects r) => RunCommandArgs -> Sem r ()
runInAnoma runArgs = do
  res <- runNock ParsedArgsModeJammedOrPretty (runArgs ^. runCommandProgramFile) (runArgs ^. runCommandArgsFile)
  let traces = res ^. runNockmaTraces
  renderStdOutLn (annotate AnnImportant $ "Traces (" <> show (length traces) <> "):")
  forM_ traces $ \tr ->
    renderStdOutLn (ppOutDefault tr)
  renderStdOutLn (annotate AnnImportant "Result:")
  renderStdOutLn (ppOutDefault (res ^. runNockmaResult))
