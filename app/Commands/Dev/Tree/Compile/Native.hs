module Commands.Dev.Tree.Compile.Native where

import Commands.Base
import Commands.Compile.Native.Options
import Commands.Compile.NativeWasiHelper qualified as Helper
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand ::
  (Members '[EmbedIO, App, TaggedLock] r) =>
  NativeOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts = do
  afile <- getMainAppFileFromInputFileType @('InputExtension 'FileExtJuvixTree) (opts ^. nativeCompileCommonOptions . compileInputFile)
  file <- fromAppPathFile afile
  s <- readFile file
  tab <- getRight (mapLeft JuvixError (Tree.runParser file s))
  entryPoint <-
    applyOptions opts
      <$> getEntryPoint (Just afile)
  cRes <-
    getRight
      . run
      . runReader entryPoint
      . runError @JuvixError
      $ treeToMiniC tab
  Helper.fromC (nativeHelperOptions opts) cRes
