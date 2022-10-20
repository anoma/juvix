module Commands.Dev.Core.Read where

import Commands.Base
import Commands.Dev.Core.Eval qualified as Eval
import Commands.Dev.Core.Read.Options
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Scoper qualified as Scoper
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. Members '[Embed IO, App] r => CoreReadOptions -> Sem r ()
runCommand opts = do
  s' <- embed (readFile f)
  (tab, mnode) <- getRight (mapLeft JuvixError (Core.runParser f Core.emptyInfoTable s'))
  let tab' = Core.applyTransformations (opts ^. coreReadTransformations) tab
  embed (Scoper.scopeTrace tab')
  unless (opts ^. coreReadNoPrint) (renderStdOut (Core.ppOut opts tab'))
  whenJust mnode $ doEval tab'
  where
    doEval :: Core.InfoTable -> Core.Node -> Sem r ()
    doEval tab' node = when (opts ^. coreReadEval) $ do
      embed (putStrLn "--------------------------------")
      embed (putStrLn "|            Eval              |")
      embed (putStrLn "--------------------------------")
      Eval.evalAndPrint (project opts) tab' node
    f :: FilePath
    f = opts ^. coreReadInputFile . pathPath
