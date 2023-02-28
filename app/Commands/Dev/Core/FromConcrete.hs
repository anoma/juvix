module Commands.Dev.Core.FromConcrete where

import Commands.Base
import Commands.Dev.Core.FromConcrete.Options
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation

runCommand :: forall r. Members '[Embed IO, App] r => CoreFromConcreteOptions -> Sem r ()
runCommand localOpts = do
  tab <- (^. coreResultTable) <$> runPipeline (localOpts ^. coreFromConcreteInputFile) upToCore
  path :: Path Abs File <- someBaseToAbs' (localOpts ^. coreFromConcreteInputFile . pathPath)
  let tab' :: InfoTable = Core.applyTransformations (project localOpts ^. coreFromConcreteTransformations) tab

      inInputModule :: IdentifierInfo -> Bool
      inInputModule _ | not (localOpts ^. coreFromConcreteFilter) = True
      inInputModule x = (== Just path) . (^? identifierLocation . _Just . intervalFile) $ x

      mainIdens :: [IdentifierInfo] =
        sortOn
          (^. identifierLocation)
          (filter inInputModule (toList (tab' ^. infoIdentifiers)))

      mainInfo :: Maybe IdentifierInfo
      mainInfo = do
        s <- tab' ^. infoMain
        tab' ^. infoIdentifiers . at s

      selInfo :: Maybe IdentifierInfo
      selInfo = do
        s <- localOpts ^. coreFromConcreteSymbolName
        find (^. identifierName . to (== s)) mainIdens

      goPrint :: Sem r ()
      goPrint = case localOpts ^. coreFromConcreteSymbolName of
        Just {} -> printNode (fromMaybe err (getDef selInfo))
        Nothing -> renderStdOut (Core.ppOut localOpts tab')
        where
          printNode :: (Text, Core.Node) -> Sem r ()
          printNode (name, node) = do
            renderStdOut (name <> " = ")
            renderStdOut (Core.ppOut localOpts node)
            newline
            newline

      goEval :: Sem r ()
      goEval = evalAndPrint localOpts tab' evalNode
        where
          evalNode :: Core.Node
            | isJust (localOpts ^. coreFromConcreteSymbolName) = getNode' selInfo
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
      | localOpts ^. coreFromConcreteEval -> goEval
      | otherwise -> goPrint
