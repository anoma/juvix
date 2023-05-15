module Commands.Dev.Core.Normalize where

import Commands.Base
import Commands.Dev.Core.Normalize.Options
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. (Members '[Embed IO, App] r) => CoreNormalizeOptions -> Sem r ()
runCommand opts = do
  f :: Path Abs File <- fromAppPathFile b
  s <- embed (readFile (toFilePath f))
  case Core.runParser f Core.emptyInfoTable s of
    Left err -> exitJuvixError (JuvixError err)
    Right (tab, Just node) -> do normalizeAndPrint opts tab node
    Right (_, Nothing) -> return ()
  where
    b :: AppPath File
    b = opts ^. coreNormalizeInputFile
