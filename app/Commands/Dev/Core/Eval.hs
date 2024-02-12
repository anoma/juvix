module Commands.Dev.Core.Eval where

import Commands.Base
import Commands.Dev.Core.Eval.Options
import Evaluator
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. (Members '[EmbedIO, App] r) => CoreEvalOptions -> Sem r ()
runCommand opts = do
  f :: Path Abs File <- fromAppPathFile b
  s <- readFile (toFilePath f)
  case Core.runParser f defaultModuleId mempty s of
    Left err -> exitJuvixError (JuvixError err)
    Right (tab, Just node) -> do evalAndPrint opts tab node
    Right (_, Nothing) -> return ()
  where
    b :: AppPath File
    b = opts ^. coreEvalInputFile
