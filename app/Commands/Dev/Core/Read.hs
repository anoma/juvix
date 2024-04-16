module Commands.Dev.Core.Read where

import Commands.Base
import Commands.Dev.Core.Read.Options
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Options qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Pretty
import Juvix.Compiler.Core.Scoper qualified as Scoper
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand ::
  forall r a.
  ( Members '[EmbedIO, App] r,
    CanonicalProjection a Eval.EvalOptions,
    CanonicalProjection a Pretty.Options,
    CanonicalProjection a CoreReadOptions
  ) =>
  a ->
  Sem r ()
runCommand opts = do
  gopts <- askGlobalOptions
  inputFile :: Path Abs File <- fromAppPathFile sinputFile
  s' <- readFile inputFile
  tab <- getRight (Core.runParserMain inputFile defaultModuleId mempty s')
  let r = run $ runReader (project @GlobalOptions @Core.CoreOptions gopts) $ runError @JuvixError $ Core.applyTransformations (project opts ^. coreReadTransformations) (Core.moduleFromInfoTable tab)
  tab0 <- getRight r
  let tab' = Core.computeCombinedInfoTable $ if project opts ^. coreReadNoDisambiguate then tab0 else Core.disambiguateNames tab0
  Scoper.scopeTrace tab'
  unless (project opts ^. coreReadNoPrint) $ do
    renderStdOut (Pretty.ppOut opts tab')
  whenJust (tab' ^. Core.infoMain) $ \sym -> doEval gopts tab' (fromJust $ tab' ^. Core.identContext . at sym)
  where
    doEval :: GlobalOptions -> Core.InfoTable -> Core.Node -> Sem r ()
    doEval gopts tab' node
      | project opts ^. coreReadEval = do
          putStrLn "--------------------------------"
          putStrLn "|            Eval              |"
          putStrLn "--------------------------------"
          Eval.evalAndPrint gopts opts tab' node
      | project opts ^. coreReadNormalize = do
          putStrLn "--------------------------------"
          putStrLn "|         Normalize            |"
          putStrLn "--------------------------------"
          let eopts :: Eval.EvalOptions = project opts
          Eval.normalizeAndPrint' eopts gopts opts tab' node
      | otherwise = return ()
    sinputFile :: AppPath File
    sinputFile = project opts ^. coreReadInputFile
