module Commands.Dev.DevCompile.Tree where

import Commands.Base
import Commands.Dev.DevCompile.Tree.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Pretty

runCommand ::
  (Members '[App, TaggedLock, EmbedIO] r) =>
  TreeOptions 'InputMain ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. treeCompileCommonOptions . compileInputFile
      moutputFile = opts ^. treeCompileCommonOptions . compileOutputFile
  outFile :: Path Abs File <- getOutputFile FileExtJuvixTree inputFile moutputFile
  res :: InfoTable <- runPipeline inputFile upToTree
  let txt = ppPrint res res
  writeFileEnsureLn outFile txt
