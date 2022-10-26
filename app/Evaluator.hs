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
import Text.Megaparsec.Pos qualified as M

data EvalOptions = EvalOptions
  { _evalInputFile :: Path,
    _evalNoIO :: Bool
  }

makeLenses ''EvalOptions

doEval ::
  forall r.
  Members '[Embed IO] r =>
  Bool ->
  Interval ->
  Core.InfoTable ->
  Core.Node ->
  Sem r (Either Core.CoreError Core.Node)
doEval noIO loc tab node
  | noIO = embed $ Core.catchEvalError loc (Core.eval (tab ^. Core.identContext) [] node)
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
  r <- doEval (project opts ^. evalNoIO) defaultLoc tab node
  case r of
    Left err -> exitJuvixError (JuvixError err)
    Right node'
      | Info.member Info.kNoDisplayInfo (Core.getInfo node') ->
          return ()
    Right node' -> do
      renderStdOut (Core.ppOut opts node')
      embed (putStrLn "")
  where
    defaultLoc :: Interval
    defaultLoc = singletonInterval (mkLoc 0 (M.initialPos f))
    f :: FilePath
    f = project opts ^. evalInputFile . pathPath
