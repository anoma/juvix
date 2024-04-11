module Commands.Dev.Tree.Compile.Wasi where

import Commands.Base
import Commands.Compile.NativeWasiHelper qualified as Helper
import Commands.Compile.Wasi.Options
import Commands.Dev.Tree.Compile.TreeToC qualified as TreeToC

runCommand ::
  forall r.
  (Members '[EmbedIO, App, TaggedLock] r) =>
  WasiOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts =
  TreeToC.treeToC (opts ^. wasiCompileCommonOptions) >>= Helper.fromC (wasiHelperOptions opts)
