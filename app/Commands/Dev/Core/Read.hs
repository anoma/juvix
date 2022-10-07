module Commands.Dev.Core.Read where

import Commands.Base
import Commands.Dev.Core.Eval qualified as Eval
import Commands.Dev.Core.Read.Options
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. Members '[Embed IO, App] r => CoreReadOptions -> Sem r ()
runCommand opts = do
  s' <- embed (readFile f)
  tab <- getRight (fst <$> mapLeft JuvixError (Core.runParser "" f Core.emptyInfoTable s'))
  let tab' = Core.applyTransformations (opts ^. coreReadTransformations) tab
  unless (opts ^. coreReadNoPrint) (renderStdOut (Core.ppOut opts tab'))
  doEval
  where
    doEval :: Sem r ()
    doEval = when (opts ^. coreReadEval) $ do
      embed (putStrLn "--------------------------------")
      embed (putStrLn "|            Eval              |")
      embed (putStrLn "--------------------------------")
      Eval.runCommand (project opts)
    f :: FilePath
    f = opts ^. coreReadInputFile . pathPath
