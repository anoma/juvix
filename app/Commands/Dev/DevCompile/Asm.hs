module Commands.Dev.DevCompile.Asm where

import Commands.Base
import Commands.Dev.DevCompile.Asm.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Pretty

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => AsmOptions 'InputMain -> Sem r ()
runCommand opts = do
  let inputFile = opts ^. asmCompileCommonOptions . compileInputFile
      moutputFile = opts ^. asmCompileCommonOptions . compileOutputFile
  outFile :: Path Abs File <- getOutputFile FileExtJuvixAsm inputFile moutputFile
  res :: InfoTable <- runPipeline opts inputFile upToAsm
  let txt = ppPrint res res
  writeFileEnsureLn outFile txt
