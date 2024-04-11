module Commands.Dev.Tree.Compile.Native where

import Commands.Base
import Commands.Compile.Native.Options
import Commands.Compile.NativeWasiHelper qualified as Helper
import Commands.Dev.Tree.Compile.TreeToC qualified as TreeToC

runCommand ::
  (Members '[EmbedIO, App, TaggedLock] r) =>
  NativeOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts =
  TreeToC.treeToC (opts ^. nativeCompileCommonOptions)
    >>= Helper.fromC (nativeHelperOptions opts)
