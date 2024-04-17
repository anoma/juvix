module Commands.Typecheck where

import Commands.Base
import Commands.Typecheck.Options

runCommand :: (Members '[EmbedIO, TaggedLock, App] r) => TypecheckOptions -> Sem r ()
runCommand localOpts = do
  void (runPipelineNoOptions (localOpts ^. typecheckInputFile) upToCoreTypecheck)
  say "Well done! It type checks"
