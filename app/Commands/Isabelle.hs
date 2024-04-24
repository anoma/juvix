module Commands.Isabelle where

import Commands.Base
import Commands.Isabelle.Options
import Juvix.Compiler.Backend.Isabelle.Data.Result
import Juvix.Compiler.Backend.Isabelle.Pretty

runCommand ::
  (Members '[EmbedIO, TaggedLock, App] r) =>
  IsabelleOptions ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. isabelleInputFile
  res <- runPipelineNoOptions inputFile upToIsabelle
  let thy = res ^. resultTheory
  renderStdOut (ppOutDefault thy)
  putStrLn ""
