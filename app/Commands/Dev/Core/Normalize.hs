module Commands.Dev.Core.Normalize where

import Commands.Base
import Commands.Dev.Core.Normalize.Options
import Evaluator
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. (Members '[EmbedIO, App] r) => CoreNormalizeOptions -> Sem r ()
runCommand opts = do
  f :: Path Abs File <- fromAppPathFile (opts ^. coreNormalizeInputFile)
  s <- readFile f
  gopts <- askGlobalOptions
  case Core.runParser f defaultModuleId mempty s of
    Left err -> exitJuvixError (JuvixError err)
    Right (tab, Just node) -> do
      let eopts :: EvalOptions = project opts
      normalizeAndPrint' eopts gopts opts tab node
    Right (_, Nothing) -> return ()
