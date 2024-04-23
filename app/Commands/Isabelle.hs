module Commands.Isabelle where

import Commands.Base
import Commands.Isabelle.Options
import Juvix.Compiler.Backend.Isabelle.Pretty
import Juvix.Compiler.Backend.Isabelle.Translation.FromTyped
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context

runCommand ::
  (Members '[EmbedIO, TaggedLock, App] r) =>
  IsabelleOptions ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. isabelleInputFile
  res <- runPipelineNoOptions inputFile upToInternalTyped
  let md = res ^. resultModule
  renderStdOut (ppOutDefault (fromInternal md))
  putStrLn ""
