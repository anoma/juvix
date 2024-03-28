module Commands.Dev.DevCompile.Casm where

import Commands.Base
import Commands.Dev.DevCompile.Casm.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Pretty

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => CasmOptions -> Sem r ()
runCommand opts = do
  let inputFile = opts ^. casmCompileCommonOptions . compileInputFile
      moutputFile = opts ^. casmCompileCommonOptions . compileOutputFile
  outFile :: Path Abs File <- getOutputFile FileExtCasm inputFile moutputFile
  res :: Result <- runPipeline inputFile upToCasm
  let txt = ppPrint res
  writeFileEnsureLn outFile txt
