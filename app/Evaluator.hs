module Evaluator where

import App
import CommonOptions
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Extra.Value qualified as Core
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Normalizer
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames qualified as Core

data EvalOptions = EvalOptions
  { _evalInputFile :: AppPath File,
    _evalNoIO :: Bool,
    _evalNoDisambiguate :: Bool,
    _evalPrintValues :: Bool
  }

makeLenses ''EvalOptions

doEvalIO ::
  Bool ->
  Interval ->
  Core.InfoTable ->
  Core.Node ->
  IO (Either Core.CoreError Core.Node)
doEvalIO noIO i tab node = runM (Core.doEval noIO i tab node)

evalAndPrint ::
  forall r a.
  (Members '[Embed IO, App] r, CanonicalProjection a EvalOptions, CanonicalProjection a Core.Options) =>
  a ->
  Core.InfoTable ->
  Core.Node ->
  Sem r ()
evalAndPrint opts tab node = do
  loc <- defaultLoc
  r <- Core.doEval (project opts ^. evalNoIO) loc tab node
  case r of
    Left err -> exitJuvixError (JuvixError err)
    Right node'
      | Info.member Info.kNoDisplayInfo (Core.getInfo node') ->
          return ()
    Right node'
      | project opts ^. evalPrintValues -> do
          renderStdOut (Core.ppOut opts (Core.toValue tab node'))
          newline
      | otherwise -> do
          renderStdOut (Core.ppOut opts node'')
          newline
      where
        node'' = if project opts ^. evalNoDisambiguate then node' else Core.disambiguateNodeNames (Core.moduleFromInfoTable tab) node'
  where
    defaultLoc :: Sem r Interval
    defaultLoc = singletonInterval . mkInitialLoc <$> fromAppPathFile f
    f :: AppPath File
    f = project opts ^. evalInputFile

normalizeAndPrint ::
  forall r a.
  (Members '[Embed IO, App] r, CanonicalProjection a EvalOptions, CanonicalProjection a Core.Options) =>
  a ->
  Core.InfoTable ->
  Core.Node ->
  Sem r ()
normalizeAndPrint opts tab node =
  let node' = normalize (Core.moduleFromInfoTable tab) node
   in if
          | Info.member Info.kNoDisplayInfo (Core.getInfo node') ->
              return ()
          | otherwise -> do
              let node'' = if project opts ^. evalNoDisambiguate then node' else Core.disambiguateNodeNames (Core.moduleFromInfoTable tab) node'
              renderStdOut (Core.ppOut opts node'')
              putStrLn ""
