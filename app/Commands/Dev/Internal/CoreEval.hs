module Commands.Dev.Internal.CoreEval where

import Commands.Base
import Commands.Dev.Internal.CoreEval.Options
import Data.HashMap.Internal.Strict (elems)
import Data.HashMap.Strict qualified as HashMap
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation
import Safe (headMay)

getSymbol :: forall r. Member App r => InfoTable -> Text -> Sem r Symbol
getSymbol tab name = maybe failAction (return . (^. identifierSymbol)) mIdent
  where
    identifiers :: [IdentifierInfo]
    identifiers = elems (tab ^. infoIdentifiers)

    mIdent :: Maybe IdentifierInfo
    mIdent = headMay (filter ((== name) . (^. identifierName)) identifiers)

    failAction :: Sem r a
    failAction = exitMsg (ExitFailure 1) (name <> " is not a function identifier")

runCommand :: (Members '[Embed IO, App] r) => InternalCoreEvalOptions -> Sem r ()
runCommand localOpts = do
  tab <- (^. coreResultTable) <$> runPipeline (localOpts ^. internalCoreEvalInputFile) upToCore
  let tab' = Core.applyTransformations (project localOpts ^. internalCoreEvalTransformations) tab
  ms <- mapM (getSymbol tab') (localOpts ^. internalCoreEvalSymbolName)
  let symbol = ms <|> (tab' ^. infoMain)
  forM_ (symbol >>= ((tab' ^. identContext) HashMap.!?)) (evalAndPrint localOpts tab')
