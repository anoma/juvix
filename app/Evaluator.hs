module Evaluator where

import App
import CommonOptions
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Error qualified as Core
import Juvix.Compiler.Core.Evaluator qualified as Core
import Juvix.Compiler.Core.Extra.Base qualified as Core
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames qualified as Core

data EvalOptions = EvalOptions
  { _evalInputFile :: AppPath File,
    _evalNoIO :: Bool,
    _evalNoDisambiguate :: Bool
  }

makeLenses ''EvalOptions

doEval ::
  forall r.
  (Members '[Embed IO] r) =>
  Bool ->
  Interval ->
  Core.InfoTable ->
  Core.Node ->
  Sem r (Either Core.CoreError Core.Node)
doEval noIO loc tab node
  | noIO = embed $ Core.catchEvalError loc (Core.eval stderr (tab ^. Core.identContext) [] node)
  | otherwise = embed $ Core.catchEvalErrorIO loc (Core.evalIO (tab ^. Core.identContext) [] node)

doEvalIO ::
  Bool ->
  Interval ->
  Core.InfoTable ->
  Core.Node ->
  IO (Either Core.CoreError Core.Node)
doEvalIO noIO i tab node = runM (doEval noIO i tab node)

evalAndPrint ::
  forall r a.
  (Members '[Embed IO, App] r, CanonicalProjection a EvalOptions, CanonicalProjection a Core.Options) =>
  a ->
  Core.InfoTable ->
  Core.Node ->
  Sem r ()
evalAndPrint opts tab node = do
  loc <- defaultLoc
  r <- doEval (project opts ^. evalNoIO) loc tab node
  case r of
    Left err -> exitJuvixError (JuvixError err)
    Right node'
      | Info.member Info.kNoDisplayInfo (Core.getInfo node') ->
          return ()
    Right node' -> do
      renderStdOut (Core.ppOut opts node'')
      embed (putStrLn "")
      where
        node'' = if project opts ^. evalNoDisambiguate then node' else Core.disambiguateNodeNames tab node'
  where
    defaultLoc :: Sem r Interval
    defaultLoc = singletonInterval . mkInitialLoc <$> someBaseToAbs' f
    f :: SomeBase File
    f = project opts ^. evalInputFile . pathPath
