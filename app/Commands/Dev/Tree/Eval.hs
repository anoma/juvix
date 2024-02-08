module Commands.Dev.Tree.Eval where

import Commands.Base
import Commands.Dev.Tree.Eval.Options
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree
import TreeEvaluator

runCommand :: forall r. (Members '[Embed IO, App] r) => TreeEvalOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile (toFilePath afile)
  case Tree.runParser (toFilePath afile) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> evalTree (opts ^. treeEvalEvaluator) tab
  where
    file :: AppPath File
    file = opts ^. treeEvalInputFile
