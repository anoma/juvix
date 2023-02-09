module Commands.Dev.Core.FromSource where

import Commands.Base
import Commands.Dev.Core.FromSource.Options
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation

runCommand :: forall r. Members '[Embed IO, App] r => CoreFromSourceOptions -> Sem r ()
runCommand localOpts = do
  tab <- (^. coreResultTable) <$> runPipeline (localOpts ^. coreFromSourceInputFile) upToCore
  path :: Path Abs File <- someBaseToAbs' (localOpts ^. coreFromSourceInputFile . pathPath)
  let tab' :: InfoTable = Core.applyTransformations (project localOpts ^. coreFromSourceTransformations) tab

      inInputModule :: IdentifierInfo -> Bool
      inInputModule = (== Just path) . (^? identifierLocation . _Just . intervalFile)

      mainIdens :: [IdentifierInfo] =
        sortOn (^. identifierLocation)
        (filter inInputModule (toList (tab' ^. infoIdentifiers)))

      mainInfo :: Maybe IdentifierInfo
      mainInfo = do
        s <- tab' ^. infoMain
        tab' ^. infoIdentifiers . at s

      selInfo :: Maybe IdentifierInfo
      selInfo = do
        s <- localOpts ^. coreFromSourceSymbolName
        find (^. identifierName . to (== s)) mainIdens

      goPrint :: Sem r ()
      goPrint = forM_ nodes printNode
        where
          printNode :: (Text, Core.Node) -> Sem r ()
          printNode (name, node) = do
            renderStdOut (name <> " = ")
            renderStdOut (Core.ppOut localOpts node)
            newline
            newline
          nodes :: [(Text, Core.Node)]
            | isJust (localOpts ^. coreFromSourceSymbolName) = [fromMaybe err (getDef selInfo)]
            | otherwise = mapMaybe (getDef . Just) mainIdens

      goEval :: Sem r ()
      goEval = evalAndPrint localOpts tab' evalNode
        where
          evalNode :: Core.Node
            | isJust (localOpts ^. coreFromSourceSymbolName) = getNode' selInfo
            | otherwise = getNode' mainInfo

      getDef :: Maybe IdentifierInfo -> Maybe (Text, Core.Node)
      getDef i = (getName' i,) <$> getNode i

      getName' :: Maybe IdentifierInfo -> Text
      getName' m = fromMaybe err m ^. identifierName

      getNode' :: Maybe IdentifierInfo -> Core.Node
      getNode' m = fromMaybe err (getNode m)

      getNode :: Maybe IdentifierInfo -> Maybe Core.Node
      getNode m = m >>= \i -> tab' ^. identContext . at (i ^. identifierSymbol)

      err :: a
      err = error "function not found"

  if
      | localOpts ^. coreFromSourceEval -> goEval
      | otherwise -> goPrint
