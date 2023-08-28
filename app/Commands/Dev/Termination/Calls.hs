module Commands.Dev.Termination.Calls where

import Commands.Base
import Commands.Dev.Termination.Calls.Options
import Juvix.Compiler.Internal.Data.InfoTable qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination qualified as Termination

runCommand :: (Members '[Embed IO, App] r) => CallsOptions -> Sem r ()
runCommand localOpts@CallsOptions {..} = do
  globalOpts <- askGlobalOptions
  results <- runPipeline _callsInputFile upToInternal
  let topModules = results ^. Internal.resultModules
      infotable = Internal.buildTable topModules
      callMap0 = Termination.buildCallMap infotable (head topModules)
      callMap = case _callsFunctionNameFilter of
        Nothing -> callMap0
        Just f -> Termination.filterCallMap f callMap0
  renderStdOut (Internal.ppOut (globalOpts, localOpts) callMap)
  newline
