module Commands.Dev.Tree.Compile.TreeToC where

import Commands.Base
import Commands.Compile.CommonOptions
import Juvix.Compiler.Backend.C
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

data TreeToCArgs = TreeToCArgs
  { _treeToCTarget :: Target,
    _treeToCCommonOptions :: CompileCommonOptions ('InputExtension 'FileExtJuvixTree)
  }

makeLenses ''TreeToCArgs

instance EntryPointOptions TreeToCArgs where
  applyOptions opts =
    set entryPointTarget (Just (opts ^. treeToCTarget))
      . applyOptions (opts ^. treeToCCommonOptions)

treeToC ::
  forall r.
  (Members '[EmbedIO, App, TaggedLock] r) =>
  TreeToCArgs ->
  Sem r MiniCResult
treeToC opts = do
  afile <-
    getMainAppFileFromInputFileType @('InputExtension 'FileExtJuvixTree)
      (opts ^. treeToCCommonOptions . compileInputFile)
  file <- fromAppPathFile afile
  s <- readFile file
  tab <- getRight (mapLeft JuvixError (Tree.runParser file s))
  entryPoint :: EntryPoint <-
    applyOptions opts
      <$> getEntryPoint (Just afile)
  getRight
    . run
    . runReader entryPoint
    . runError @JuvixError
    $ treeToMiniC tab
