module TreeEvaluator
  ( module TreeEvaluator,
    module Commands.Dev.Tree.Eval.Options,
  )
where

import App
import Commands.Dev.Tree.Eval.Options
import CommonOptions
import Juvix.Compiler.Tree.Data.InfoTable qualified as Tree
import Juvix.Compiler.Tree.Error qualified as Tree
import Juvix.Compiler.Tree.Evaluator qualified as Tree
import Juvix.Compiler.Tree.EvaluatorEff qualified as Eff
import Juvix.Compiler.Tree.EvaluatorSem qualified as TreeSem
import Juvix.Compiler.Tree.Language.Value qualified as Tree
import Juvix.Compiler.Tree.Pretty qualified as Tree

evalTree :: forall r. (Members '[EmbedIO, App] r) => Evaluator -> Tree.InfoTable -> Sem r ()
evalTree ev tab =
  case tab ^. Tree.infoMainFunction of
    Just sym -> do
      r <- doEval ev tab (Tree.lookupFunInfo tab sym)
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

doEvalDefault ::
  (MonadIO m) =>
  Tree.InfoTable ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEvalDefault = doEval defaultEvaluator

doEval ::
  (MonadIO m) =>
  Evaluator ->
  Tree.InfoTable ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEval = \case
  EvalEffectful -> doEvalEff
  EvalRaw -> doEvalRaw
  EvalSem -> doEvalSem

doEvalRaw ::
  (MonadIO m) =>
  Tree.InfoTable ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEvalRaw tab' = liftIO . Tree.catchEvalErrorIO . liftIO . Tree.hEvalIO stdin stdout tab'

doEvalEff ::
  (MonadIO m) =>
  Tree.InfoTable ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEvalEff tab' funInfo = Eff.hEvalIOEither stdin stdout tab' funInfo

doEvalSem ::
  (MonadIO m) =>
  Tree.InfoTable ->
  Tree.FunctionInfo ->
  m (Either Tree.TreeError Tree.Value)
doEvalSem tab' funInfo = TreeSem.hEvalIOEither stdin stdout tab' funInfo
