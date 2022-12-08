module Commands.Dev.Core.Read where

import Commands.Base
import Commands.Dev.Core.Read.Options
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Scoper qualified as Scoper
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r a. (Members '[Embed IO, App] r, CanonicalProjection a Eval.EvalOptions, CanonicalProjection a Core.Options, CanonicalProjection a CoreReadOptions) => a -> Sem r ()
runCommand opts = do
  s' <- embed (readFile f)
  (tab, mnode) <- getRight (mapLeft JuvixError (Core.runParser f Core.emptyInfoTable s'))
  let tab' = Core.applyTransformations (project opts ^. coreReadTransformations) tab
  embed (Scoper.scopeTrace tab')
  unless (project opts ^. coreReadNoPrint) $ do
    renderStdOut (Core.ppOut opts tab')
  whenJust mnode $ doEval tab'
  where
    doEval :: Core.InfoTable -> Core.Node -> Sem r ()
    doEval tab' node =
      if
          | project opts ^. coreReadEval -> do
              embed (putStrLn "--------------------------------")
              embed (putStrLn "|            Eval              |")
              embed (putStrLn "--------------------------------")
              Eval.evalAndPrint opts tab' node
          | otherwise -> do
              embed (putStrLn "-- Node")
              renderStdOut (Core.ppOut opts node)
              embed (putStrLn "")
    f :: FilePath
    f = project opts ^. coreReadInputFile . pathPath
