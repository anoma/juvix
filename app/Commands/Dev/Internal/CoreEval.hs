module Commands.Dev.Internal.CoreEval where

import Commands.Base
import Commands.Dev.Internal.CoreEval.Options
import Data.HashMap.Strict qualified as HashMap
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation

runCommand :: (Members '[Embed IO, App] r) => InternalCoreEvalOptions -> Sem r ()
runCommand localOpts = do
  tab <- (^. coreResultTable) <$> runPipeline (localOpts ^. internalCoreEvalInputFile) upToCore
  let tab' = Core.applyTransformations (project localOpts ^. internalCoreEvalTransformations) tab
  forM_ ((tab' ^. infoMain) >>= ((tab' ^. identContext) HashMap.!?)) (evalAndPrint localOpts tab')
