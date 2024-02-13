module Commands.Dev.Tree.FromAsm where

import Commands.Base
import Commands.Dev.Tree.FromAsm.Options
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm
import Juvix.Compiler.Tree.Data.InfoTable qualified as Tree
import Juvix.Compiler.Tree.Error (TreeError)
import Juvix.Compiler.Tree.Pretty qualified as Tree
import Juvix.Compiler.Tree.Translation.FromAsm qualified as Tree

runCommand :: forall r. (Members '[EmbedIO, App] r) => TreeFromAsmOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile (toFilePath afile)
  case Asm.runParser (toFilePath afile) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> do
      r :: Either JuvixError Tree.InfoTable <- runError $ mapError (JuvixError @TreeError) $ Tree.fromAsm tab
      tab' <- getRight r
      renderStdOut (Tree.ppOutDefault tab' tab')
  where
    file :: AppPath File
    file = opts ^. treeFromAsmInputFile
