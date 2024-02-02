module TreeEvaluator where

import App
import CommonOptions
import Juvix.Compiler.Tree.Data.InfoTable qualified as Tree
import Juvix.Compiler.Tree.Error qualified as Tree
import Juvix.Compiler.Tree.Evaluator qualified as Tree
import Juvix.Compiler.Tree.Language.Value qualified as Tree
import Juvix.Compiler.Tree.Pretty qualified as Tree

evalTree :: forall r. (Members '[Embed IO, App] r) => Tree.InfoTable -> Sem r ()
evalTree tab =
  case tab ^. Tree.infoMainFunction of
    Just sym -> do
      r <- liftIO $ doEval tab (Tree.lookupFunInfo tab sym)
      case r of
        Left err ->
          exitJuvixError (JuvixError err)
        Right Tree.ValVoid ->
          return ()
        Right val -> do
          renderStdOut (Tree.ppOutDefault tab val)
          putStrLn ""
    Nothing ->
      exitMsg (ExitFailure 1) "no 'main' function"

doEval ::
  (MonadIO m) =>
  Tree.InfoTable ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEval tab' funInfo =
  liftIO $ Tree.catchEvalErrorIO (liftIO $ Tree.hEvalIO stdin stdout tab' funInfo)
