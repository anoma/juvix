module Commands.Dev.Termination.Calls where

import Commands.Base
import Commands.Dev.Termination.Calls.Options
import Juvix.Compiler.Abstract.Pretty qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete qualified as Abstract
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination qualified as Termination

runCommand :: Members '[Embed IO, App] r => CallsOptions -> Sem r ()
runCommand localOpts@CallsOptions {..} = do
  globalOpts <- askGlobalOptions
  results <- runPipeline _callsInputFile upToAbstract
  let topModule = head (results ^. Abstract.resultModules)
      infotable = results ^. Abstract.resultTable
      callMap0 = Termination.buildCallMap infotable topModule
      callMap = case _callsFunctionNameFilter of
        Nothing -> callMap0
        Just f -> Termination.filterCallMap f callMap0
  renderStdOut (Abstract.ppOut (globalOpts, localOpts) callMap)
  newline
