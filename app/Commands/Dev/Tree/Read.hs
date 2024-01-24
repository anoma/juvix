module Commands.Dev.Tree.Read where

import Commands.Base
import Commands.Dev.Tree.Read.Options
import Juvix.Compiler.Tree.Pretty qualified as Tree
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand :: forall r. (Members '[Embed IO, App] r) => TreeReadOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile (toFilePath afile)
  case Tree.runParser (toFilePath afile) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> renderStdOut (Tree.ppOutDefault tab tab)
  where
    file :: AppPath File
    file = opts ^. treeReadInputFile
