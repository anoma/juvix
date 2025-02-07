module Evaluator where

import App
import CommonOptions
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Extra.Value qualified as Core
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Normalizer
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames qualified as Core

data EvalOptions = EvalOptions
  { _evalInputFile :: AppPath File,
    _evalNoIO :: Bool,
    _evalNoDisambiguate :: Bool,
    _evalPrintValues :: Bool
  }

makeLenses ''EvalOptions

evalAndPrint' ::
  forall r.
  (Members '[EmbedIO, App] r) =>
  Core.CoreOptions ->
  Core.Options ->
  EvalOptions ->
  Core.InfoTable ->
  Core.Node ->
  Sem r ()
evalAndPrint' gopts copts eopts tab0 node = do
  md <- getRight . run . runError @JuvixError . runReader gopts $ Core.applyTransformations Core.toEvalTransformations (Core.moduleFromInfoTable tab0)
  let tab = Core.computeCombinedInfoTable md
  loc <- defaultLoc
  r <- Core.doEval (Just $ project gopts ^. Core.optFieldSize) (eopts ^. evalNoIO) loc tab node
  case r of
    Left err -> exitJuvixError (JuvixError err)
    Right node'
      | Info.member Info.kNoDisplayInfo (Core.getInfo node') ->
          return ()
    Right node'
      | eopts ^. evalPrintValues -> do
          renderStdOut (Core.ppOut copts (Core.toValue tab node'))
          newline
      | otherwise -> do
          renderStdOut (Core.ppOut copts node'')
          newline
      where
        node'' = if project eopts ^. evalNoDisambiguate then node' else Core.disambiguateNodeNames (Core.moduleFromInfoTable tab) node'
  where
    defaultLoc :: Sem r Interval
    defaultLoc = singletonInterval . mkInitialLoc <$> fromAppPathFile f

    f :: AppPath File
    f = eopts ^. evalInputFile

evalAndPrint ::
  forall r evalOptions coreOptions.
  ( Members '[EmbedIO, App] r,
    CanonicalProjection coreOptions Core.CoreOptions,
    CanonicalProjection evalOptions EvalOptions,
    CanonicalProjection evalOptions Core.Options
  ) =>
  coreOptions ->
  evalOptions ->
  Core.InfoTable ->
  Core.Node ->
  Sem r ()
evalAndPrint gopts opts = evalAndPrint' (project gopts) (project opts) (project opts)

normalizeAndPrint ::
  forall r evalOptions coreOptions.
  ( Members '[EmbedIO, App] r,
    CanonicalProjection evalOptions EvalOptions,
    CanonicalProjection coreOptions Core.CoreOptions,
    CanonicalProjection coreOptions Core.Options
  ) =>
  evalOptions ->
  coreOptions ->
  Core.InfoTable ->
  Core.Node ->
  Sem r ()
normalizeAndPrint eopts copts = normalizeAndPrint' eopts copts copts

normalizeAndPrint' ::
  forall r evalOptions coreOptions corePretty.
  ( Members '[EmbedIO, App] r,
    CanonicalProjection evalOptions EvalOptions,
    CanonicalProjection coreOptions Core.CoreOptions,
    CanonicalProjection corePretty Core.Options
  ) =>
  evalOptions ->
  coreOptions ->
  corePretty ->
  Core.InfoTable ->
  Core.Node ->
  Sem r ()
normalizeAndPrint' eopts_ copts_ popts_ tab node =
  let eopts :: EvalOptions = project eopts_
      copts :: Core.CoreOptions = project copts_
      popts :: Core.Options = project popts_
      node' = normalize (copts ^. Core.optFieldSize) (Core.moduleFromInfoTable tab) node
   in unless (Info.member Info.kNoDisplayInfo (Core.getInfo node')) $ do
        let node'' = if eopts ^. evalNoDisambiguate then node' else Core.disambiguateNodeNames (Core.moduleFromInfoTable tab) node'
        renderStdOut (Core.ppOut popts node'')
        putStrLn ""
