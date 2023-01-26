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
  inputFile :: Path Abs File <- someBaseToAbs' sinputFile
  s' <- embed . readFile . toFilePath $ inputFile
  tab <- getRight (mapLeft JuvixError (Core.runParserMain inputFile Core.emptyInfoTable s'))
  let tab' = Core.applyTransformations (project opts ^. coreReadTransformations) tab
  embed (Scoper.scopeTrace tab')
  unless (project opts ^. coreReadNoPrint) $ do
    renderStdOut (Core.ppOut opts tab')
  whenJust (tab' ^. Core.infoMain) $ \sym -> doEval tab' (fromJust $ tab' ^. Core.identContext . at sym)
  where
    doEval :: Core.InfoTable -> Core.Node -> Sem r ()
    doEval tab' node =
      if
          | project opts ^. coreReadEval -> do
              embed (putStrLn "--------------------------------")
              embed (putStrLn "|            Eval              |")
              embed (putStrLn "--------------------------------")
              Eval.evalAndPrint opts tab' node
          | otherwise -> return ()
    sinputFile :: SomeBase File
    sinputFile = project opts ^. coreReadInputFile . pathPath
