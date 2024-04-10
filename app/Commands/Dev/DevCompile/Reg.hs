module Commands.Dev.DevCompile.Reg where

import Commands.Base
import Commands.Dev.DevCompile.Reg.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Pretty

runCommand ::
  (Members '[App, EmbedIO, TaggedLock] r) =>
  RegOptions 'InputMain ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. regCompileCommonOptions . compileInputFile
      moutputFile = opts ^. regCompileCommonOptions . compileOutputFile
  outFile :: Path Abs File <- getOutputFile FileExtJuvixReg inputFile moutputFile
  res :: InfoTable <- runPipeline inputFile upToReg
  let txt = ppPrint res res
  writeFileEnsureLn outFile txt
