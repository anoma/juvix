module Commands.Typecheck where

import Commands.Base
import Commands.Typecheck.Options

runCommand :: (Members AppEffects r) => TypecheckOptions -> Sem r ()
runCommand localOpts = do
  case localOpts ^. typecheckInputFile of
    Just _inputFile -> void (runPipelineNoOptions (localOpts ^. typecheckInputFile) upToCoreTypecheck)
    Nothing -> void (runPipelineOptions . runPipelineSetup $ processProject)
  logInfo "Well done! It type checks"
