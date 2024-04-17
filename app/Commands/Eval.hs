module Commands.Eval where

import Commands.Base
import Commands.Eval.Options
import Evaluator qualified as Eval
import Juvix.Compiler.Core qualified as Core
import Juvix.Extra.Strings qualified as Str

runCommand :: (Members '[EmbedIO, TaggedLock, App] r) => EvalOptions -> Sem r ()
runCommand opts@EvalOptions {..} = do
  gopts <- askGlobalOptions
  Core.CoreResult {..} <- runPipelineNoOptions _evalInputFile upToCore
  let r =
        run
          . runReader (project gopts)
          . runError @JuvixError
          $ (Core.toStored' _coreResultModule :: Sem '[Error JuvixError, Reader Core.CoreOptions] Core.Module)
  tab <- Core.computeCombinedInfoTable <$> getRight r
  let mevalNode
        | isJust _evalSymbolName = getNode tab (selInfo tab)
        | otherwise = getNode tab (mainInfo tab)
  case mevalNode of
    Just evalNode -> do
      evopts <- evalOptionsToEvalOptions opts
      Eval.evalAndPrint' (project gopts) (project opts) evopts tab evalNode
    Nothing -> do
      let name = fromMaybe Str.main _evalSymbolName
      exitFailMsg ("function not found: " <> name)
  where
    mainInfo :: Core.InfoTable -> Maybe Core.IdentifierInfo
    mainInfo tab = do
      s <- tab ^. Core.infoMain
      tab ^. Core.infoIdentifiers . at s

    selInfo :: Core.InfoTable -> Maybe Core.IdentifierInfo
    selInfo tab = do
      s <- _evalSymbolName
      find (^. Core.identifierName . to (== s)) (tab ^. Core.infoIdentifiers)

    getNode :: Core.InfoTable -> Maybe Core.IdentifierInfo -> Maybe Core.Node
    getNode tab m = m >>= \i -> tab ^. Core.identContext . at (i ^. Core.identifierSymbol)
