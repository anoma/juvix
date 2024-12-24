module Commands.Dev.Nockma.Run.Anoma where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Commands.Dev.Anoma.Base
import Commands.Dev.Anoma.Prove.Options.ProveArg
import Juvix.Compiler.Nockma.Pretty

data RunCommandArgs = RunCommandArgs
  { _runCommandArgs :: [ProveArg],
    _runCommandProgramFile :: AppPath File
  }

makeLenses ''RunCommandArgs

runInAnoma :: forall r. (Members '[Error SimpleError, Anoma] r, Members AppEffects r) => RunCommandArgs -> Sem r ()
runInAnoma runArgs = do
  res <- runNock (runArgs ^. runCommandProgramFile) (runArgs ^. runCommandArgs)
  let traces = res ^. runNockmaTraces
  renderStdOutLn (annotate AnnImportant $ "Traces (" <> show (length traces) <> "):")
  forM_ traces $ \tr ->
    renderStdOutLn (ppOutDefault tr)
  renderStdOutLn (annotate AnnImportant "Result:")
  renderStdOutLn (ppOutDefault (res ^. runNockmaResult))
