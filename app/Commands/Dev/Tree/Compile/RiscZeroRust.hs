module Commands.Dev.Tree.Compile.RiscZeroRust where

import Commands.Base
import Commands.Compile.RiscZeroRust.Options
import Commands.Compile.RiscZeroRust.Rust
import Juvix.Compiler.Backend.Rust.Data.Result
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand ::
  (Members '[App, TaggedLock, EmbedIO] r) =>
  RiscZeroRustOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts = do
  let inputFile = Just $ opts ^. riscZeroRustCompileCommonOptions . compileInputFile
  mainFile <- getMainFile inputFile
  tab :: InfoTable <- readFile mainFile >>= getRight . Tree.runParser mainFile
  entrypoint <-
    applyOptions opts
      <$> getEntryPoint inputFile
  Result {..} <-
    getRight
      . run
      . runError @JuvixError
      . runReader entrypoint
      $ treeToRiscZeroRust tab
  compileRustCode opts inputFile _resultRustCode
