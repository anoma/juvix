module Commands.Dev.Tree.Compile.Casm where

import Commands.Base
import Commands.Dev.DevCompile.Casm.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Casm.Pretty qualified as Casm
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => CasmOptions ('InputExtension 'FileExtJuvixTree) -> Sem r ()
runCommand opts = do
  let opts' = opts ^. casmCompileCommonOptions
      inputFile = Just (opts' ^. compileInputFile)
      moutputFile = opts' ^. compileOutputFile
  outFile <- getOutputFile FileExtCasm inputFile moutputFile
  mainFile <- getMainFile inputFile
  tab :: InfoTable <- readFile mainFile >>= getRight . Tree.runParser mainFile
  entrypoint <-
    set entryPointTarget (Just TargetCairo) -- TODO revise
      . applyOptions opts
      <$> getEntryPoint inputFile
  res <-
    getRight
      . run
      . runError @JuvixError
      . runReader entrypoint
      $ treeToCasm tab
  writeFileEnsureLn outFile (Casm.ppPrint res)
