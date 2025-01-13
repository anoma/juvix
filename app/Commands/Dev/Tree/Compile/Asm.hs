module Commands.Dev.Tree.Compile.Asm where

import Commands.Base
import Commands.Dev.DevCompile.Asm.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => AsmOptions ('InputExtension 'FileExtJuvixTree) -> Sem r ()
runCommand opts = do
  let inputFile = opts ^. asmCompileCommonOptions . compileInputFile
      moutputFile = opts ^. asmCompileCommonOptions . compileOutputFile
  outFile <- getOutputFile FileExtJuvixAsm (Just inputFile) moutputFile
  mainFile <- getMainFile (Just inputFile)
  tab :: InfoTable <- readFile mainFile >>= getRight . Tree.runParser mainFile
  ep <- getEntryPoint (Just inputFile)
  res <-
    getRight
      . run
      . runReader ep
      . runError @JuvixError
      $ treeToAsm tab
  writeFileEnsureLn outFile (Asm.ppPrint res res)
