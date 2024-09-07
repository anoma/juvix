module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
  ( Termination,
    buildCallMap,
    checkTerminationShallow,
    runTermination,
    evalTermination,
    execTermination,
    functionSafeToNormalize,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.TerminationState,
  )
where

import Juvix.Compiler.Internal.Language as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.TerminationState
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.LexOrder
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.ScanFunctionCalls
import Juvix.Prelude

data Termination :: Effect where
  CheckTerminationShallow :: (Scannable a) => a -> Termination m ()
  FunctionTermination :: FunctionName -> Termination m IsTerminating

makeSem ''Termination

functionSafeToNormalize :: (Members '[Termination] r) => FunctionName -> Sem r Bool
functionSafeToNormalize = fmap safeToNormalize . functionTermination

runTermination :: forall r a. (Members '[Error JuvixError] r) => TerminationState -> Sem (Termination ': r) a -> Sem r (TerminationState, a)
runTermination ini m = do
  res <- runTerminationState ini m
  checkNonTerminating (fst res)
  return res
  where
    checkNonTerminating :: TerminationState -> Sem r ()
    checkNonTerminating i =
      whenJust (i ^. terminationFailedSet . to (nonEmpty . toList)) $
        throw . JuvixError . ErrNoLexOrder . NoLexOrder

evalTermination :: (Members '[Error JuvixError] r) => TerminationState -> Sem (Termination ': r) a -> Sem r a
evalTermination s = fmap snd . runTermination s

execTermination :: (Members '[Error JuvixError] r) => TerminationState -> Sem (Termination ': r) a -> Sem r TerminationState
execTermination s = fmap fst . runTermination s

runTerminationState :: TerminationState -> Sem (Termination ': r) a -> Sem r (TerminationState, a)
runTerminationState ini = reinterpret (runState ini) $ \case
  CheckTerminationShallow m -> checkTerminationShallow' m
  FunctionTermination m -> functionTermination' m

-- | If the function is missing, can we assume that it is not recursive
functionTermination' ::
  forall r.
  (Members '[State TerminationState] r) =>
  FunctionName ->
  Sem r IsTerminating
functionTermination' f = fromMaybe TerminatingChecked <$> gets (^. terminationTable . at f)

-- | Returns the set of non-terminating functions. Does not go into imports.
checkTerminationShallow' ::
  forall r m.
  (Members '[State TerminationState] r, Scannable m) =>
  m ->
  Sem r ()
checkTerminationShallow' topModule = do
  let (callmap, scannedFuns) = buildCallMap topModule
  forM_ (callMapRecursiveBehaviour callmap) $ \rb -> do
    let funName = rb ^. recursiveBehaviourFun
        markedTerminating :: Bool = funInfo ^. Internal.funDefTerminating
        funInfo :: FunctionDef
        funInfo = fromMaybe err (scannedFuns ^. at funName)
          where
            err = error ("Impossible: function not found: " <> funName ^. nameText)
        order = findOrder rb
    addTerminating funName $
      if
          | Just {} <- order -> TerminatingChecked
          | markedTerminating -> TerminatingFailedMarked
          | Nothing <- order -> TerminatingFailed
