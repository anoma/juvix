module Commands.Dev.Termination.CallGraph where

import Commands.Base
import Commands.Dev.Termination.CallGraph.Options
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination qualified as Termination
import Juvix.Compiler.Store.Extra qualified as Stored
import Juvix.Prelude.Pretty

runCommand :: (Members '[Embed IO, TaggedLock, App] r) => CallGraphOptions -> Sem r ()
runCommand CallGraphOptions {..} = do
  globalOpts <- askGlobalOptions
  PipelineResult {..} <- runPipelineTermination _graphInputFile upToInternal
  let mainModule = _pipelineResult ^. Internal.resultModule
      toAnsiText' :: forall a. (HasAnsiBackend a, HasTextBackend a) => a -> Text
      toAnsiText' = toAnsiText (not (globalOpts ^. globalNoColors))
      infotable =
        Internal.computeCombinedInfoTable (Stored.getInternalModuleTable _pipelineResultImports)
          <> _pipelineResult ^. Internal.resultInternalModule . Internal.internalModuleInfoTable
      callMap = Termination.buildCallMap mainModule
      completeGraph = Termination.completeCallGraph callMap
      filteredGraph =
        maybe
          completeGraph
          (`Termination.unsafeFilterGraph` completeGraph)
          _graphFunctionNameFilter
      rEdges = Termination.reflexiveEdges filteredGraph
      recBehav = map Termination.recursiveBehaviour rEdges
  renderStdOut (Internal.ppOut globalOpts filteredGraph)
  newline
  forM_ recBehav $ \r -> do
    let funName = r ^. Termination.recursiveBehaviourFun
        funInfo =
          HashMap.lookupDefault
            impossible
            funName
            (infotable ^. Internal.infoFunctions)
        markedTerminating = funInfo ^. Internal.functionInfoTerminating
        n = toAnsiText' (Internal.ppOut globalOpts funName)
    renderStdOut (Internal.ppOut globalOpts r)
    newline
    if
        | markedTerminating ->
            printSuccessExit (n <> " Terminates by assumption")
        | otherwise ->
            case Termination.findOrder r of
              Nothing ->
                printFailureExit (n <> " Fails the termination checking")
              Just (Termination.LexOrder k) ->
                printSuccessExit (n <> " Terminates with order " <> show (toList k))
