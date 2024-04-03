module Commands.Dev.DevCompile.Core where

import Commands.Base
import Commands.Dev.DevCompile.Core.Options
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Options qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames (disambiguateNames')
import Juvix.Compiler.Core.Translation

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => CoreOptions -> Sem r ()
runCommand localOpts = do
  opts' <- fromCompileCommonOptionsMain (localOpts ^. coreCompileCommonOptions)
  gopts <- askGlobalOptions
  let inputFile = opts' ^. compileInputFile
  md <- (^. coreResultModule) <$> runPipeline (Just inputFile) upToCore
  mainFile :: Path Abs File <- getMainFile (Just inputFile)
  let r =
        run
          . runReader (project @GlobalOptions @Core.CoreOptions gopts)
          . runError @JuvixError
          $ Core.applyTransformations (project localOpts ^. coreTransformations) md
  tab0 :: InfoTable <- Core.computeCombinedInfoTable <$> getRight r
  let tab' :: InfoTable = if localOpts ^. coreNoDisambiguate then tab0 else disambiguateNames' tab0
      inInputModule :: IdentifierInfo -> Bool
      inInputModule x
        | not (localOpts ^. coreFilter) = True
        | otherwise = x ^? identifierLocation . _Just . intervalFile == Just mainFile

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
        s <- localOpts ^. coreSymbolName
        find (^. identifierName . to (== s)) mainIdens

      goPrint :: Sem r ()
      goPrint = case localOpts ^. coreSymbolName of
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
      goEval = do
        evalOpts <- coreOptionsToEvalOptions localOpts
        evalAndPrint' (project gopts) (project localOpts) evalOpts tab' evalNode
        where
          evalNode :: Core.Node
            | isJust (localOpts ^. coreSymbolName) = getNode' selInfo
            | otherwise = getNode' mainInfo

      goNormalize :: Sem r ()
      goNormalize = do
        evalOpts <- coreOptionsToEvalOptions localOpts
        normalizeAndPrint' evalOpts gopts localOpts tab' evalNode
        where
          evalNode :: Core.Node
            | isJust (localOpts ^. coreSymbolName) = getNode' selInfo
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
      | localOpts ^. coreEval -> goEval
      | localOpts ^. coreNormalize -> goNormalize
      | otherwise -> goPrint
