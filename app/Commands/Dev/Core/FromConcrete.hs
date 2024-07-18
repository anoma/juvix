module Commands.Dev.Core.FromConcrete where

import Commands.Base
import Commands.Dev.Core.FromConcrete.Options
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Options qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNames')
import Juvix.Compiler.Core.Translation

runCommand :: forall r. (Members AppEffects r) => CoreFromConcreteOptions -> Sem r ()
runCommand coreOpts = do
  gopts <- askGlobalOptions
  md <- (^. coreResultModule) <$> silenceProgressLog (runPipelineLogger () (Just (coreOpts ^. coreFromConcreteInputFile)) upToCore)
  path :: Path Abs File <- fromAppPathFile (coreOpts ^. coreFromConcreteInputFile)
  let r =
        run
          . runReader (project @GlobalOptions @Core.CoreOptions gopts)
          . runError @JuvixError
          $ Core.applyTransformations (project coreOpts ^. coreFromConcreteTransformations) md
  tab0 :: InfoTable <- Core.computeCombinedInfoTable <$> getRight r
  let tab' :: InfoTable = if coreOpts ^. coreFromConcreteNoDisambiguate then tab0 else disambiguateNames' tab0
      inInputModule :: IdentifierInfo -> Bool
      inInputModule _ | not (coreOpts ^. coreFromConcreteFilter) = True
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
        s <- coreOpts ^. coreFromConcreteSymbolName
        find (^. identifierName . to (== s)) mainIdens

      goPrint :: Sem r ()
      goPrint = case coreOpts ^. coreFromConcreteSymbolName of
        Just {} -> printNode (fromMaybe err (getDef selInfo))
        Nothing -> renderStdOut (Core.ppOut coreOpts tab')
        where
          printNode :: (Text, Core.Node) -> Sem r ()
          printNode (name, node) = do
            renderStdOut (name <> " = ")
            renderStdOut (Core.ppOut coreOpts node)
            newline
            newline

      goEval :: Sem r ()
      goEval = evalAndPrint gopts coreOpts tab' evalNode
        where
          evalNode :: Core.Node
            | isJust (coreOpts ^. coreFromConcreteSymbolName) = getNode' selInfo
            | otherwise = getNode' mainInfo

      goNormalize :: Sem r ()
      goNormalize = do
        evalOpts <- coreFromConcreteOptionsToEvalOptions coreOpts
        normalizeAndPrint' evalOpts gopts coreOpts tab' evalNode
        where
          evalNode :: Core.Node
            | isJust (coreOpts ^. coreFromConcreteSymbolName) = getNode' selInfo
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
    | coreOpts ^. coreFromConcreteEval -> goEval
    | coreOpts ^. coreFromConcreteNormalize -> goNormalize
    | otherwise -> goPrint
