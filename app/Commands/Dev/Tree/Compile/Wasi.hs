module Commands.Dev.Tree.Compile.Wasi where

import Commands.Base
import Commands.Compile.NativeWasiHelper qualified as Helper
import Commands.Compile.Wasi.Options
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand ::
  forall r.
  (Members '[EmbedIO, App, TaggedLock] r) =>
  WasiOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts = do
  afile <-
    getMainAppFileFromInputFileType @('InputExtension 'FileExtJuvixTree)
      (opts ^. wasiCompileCommonOptions . compileInputFile)
  file <- fromAppPathFile afile
  s <- readFile file
  tab <- getRight (mapLeft JuvixError (Tree.runParser file s))
  entryPoint :: EntryPoint <-
    applyOptions opts
      <$> getEntryPoint (Just afile)
  cRes <-
    getRight
      . run
      . runReader entryPoint
      . runError @JuvixError
      $ treeToMiniC tab
  Helper.fromC (wasiHelperOptions opts) cRes
