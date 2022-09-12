module Commands.Dev.Termination.CallGraph where

import Commands.Base
import Commands.Dev.Termination.CallGraph.Options
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Abstract.Language qualified as Abstract
import Juvix.Compiler.Abstract.Pretty qualified as Abstract
import Juvix.Compiler.Abstract.Translation.FromConcrete qualified as Abstract
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination qualified as Termination
import Juvix.Prelude.Pretty

runCommand :: Members '[Embed IO, App] r => EntryPoint -> CallGraphOptions -> Sem r ()
runCommand entryPoint CallGraphOptions {..} = do
  globalOpts <- askGlobalOptions
  results <- runPipeline (upToAbstract entryPoint)
  let topModule = head (results ^. Abstract.resultModules)
      toAnsiText' :: forall a. (HasAnsiBackend a, HasTextBackend a) => a -> Text
      toAnsiText' = toAnsiText (not (globalOpts ^. globalNoColors))
      infotable = results ^. Abstract.resultTable
      callMap = Termination.buildCallMap infotable topModule
      completeGraph = Termination.completeCallGraph callMap
      filteredGraph =
        maybe
          completeGraph
          (`Termination.unsafeFilterGraph` completeGraph)
          _graphFunctionNameFilter
      rEdges = Termination.reflexiveEdges filteredGraph
      recBehav = map Termination.recursiveBehaviour rEdges
  renderStdOut (Abstract.ppOut globalOpts filteredGraph)
  newline
  forM_ recBehav $ \r -> do
    let funName = r ^. Termination.recursiveBehaviourFun
        funRef = Abstract.FunctionRef funName
        funInfo =
          HashMap.lookupDefault
            impossible
            funRef
            (infotable ^. Abstract.infoFunctions)
        markedTerminating = funInfo ^. (Abstract.functionInfoDef . Abstract.funDefTerminating)
        n = toAnsiText' (Abstract.ppOut globalOpts funName)
    renderStdOut (Abstract.ppOut globalOpts r)
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
