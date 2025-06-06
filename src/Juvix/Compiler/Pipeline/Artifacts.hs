-- | Stuff that is generated when the pipeline is run
--
-- Arguments to the *Artifacts functions in this module should not modify the
-- Artifacts State since any changes will be overwritten by the
-- `runStateLikeArtifacts` wrapper.
module Juvix.Compiler.Pipeline.Artifacts
  ( module Juvix.Compiler.Pipeline.Artifacts,
    module Juvix.Compiler.Pipeline.Artifacts.Base,
  )
where

import Juvix.Compiler.Concrete.Data.InfoTableBuilder qualified as Scoped
import Juvix.Compiler.Concrete.Data.Scope qualified as S
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Core.Data.InfoTableBuilder qualified as Core
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.ResultBuilder
import Juvix.Compiler.Pipeline.Artifacts.Base
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language
import Juvix.Compiler.Store.Scoped.Data.InfoTable
import Juvix.Prelude

appendArtifactsModuleTable :: ModuleTable -> Artifacts -> Artifacts
appendArtifactsModuleTable mtab =
  over artifactInternalTypedTable (computeCombinedInfoTable importTab <>)
    . over (artifactScopeTable . infoBuiltins) (computeCombinedBuiltins mtab <>)
    . over (artifactCoreModule . Core.moduleImportsTable) (computeCombinedCoreInfoTable mtab <>)
    . over artifactModuleTable (mtab <>)
  where
    importTab :: Internal.InternalModuleTable
    importTab = getInternalModuleTable mtab

-- | It only reads the Artifacts. It does not modify the table in it.
extendedTableReplArtifacts :: forall r. (Members '[State Artifacts] r) => Internal.Expression -> Sem r Internal.InfoTable
extendedTableReplArtifacts e = Internal.extendWithReplExpression e <$> gets (^. artifactInternalTypedTable)

runCoreInfoTableBuilderArtifacts :: (Members '[State Artifacts] r) => Sem (Core.InfoTableBuilder ': r) a -> Sem r a
runCoreInfoTableBuilderArtifacts = runStateLikeArtifacts Core.runInfoTableBuilder artifactCoreModule

tmpCoreInfoTableBuilderArtifacts :: (Members '[State Artifacts] r) => Sem (Core.InfoTableBuilder ': r) a -> Sem r a
tmpCoreInfoTableBuilderArtifacts m = do
  md <- gets (^. artifactCoreModule)
  a <- runStateLikeArtifacts Core.runInfoTableBuilder artifactCoreModule m
  modify' (set artifactCoreModule md)
  return a

runScoperInfoTableBuilderArtifacts :: (Members '[Error JuvixError, State Artifacts] r) => Sem (Scoped.InfoTableBuilder ': r) a -> Sem r a
runScoperInfoTableBuilderArtifacts =
  mapError (JuvixError @ScoperError)
    . runStateLikeArtifacts Scoped.runInfoTableBuilderRepl artifactScopeTable
    . inject

runScoperScopeArtifacts :: (Members '[State Artifacts] r) => Sem (State S.Scope ': r) a -> Sem r a
runScoperScopeArtifacts m = do
  s <- fromJust <$> gets (^. artifactMainModuleScope)
  (s', a) <- runState s m
  modify' (set artifactMainModuleScope (Just s'))
  return a

runNameIdGenArtifacts ::
  (Members '[State Artifacts] r) =>
  Sem (NameIdGen ': r) a ->
  Sem r a
runNameIdGenArtifacts = runStateLikeArtifacts runNameIdGen artifactNameIdState

readerFunctionsTableArtifacts :: (Members '[State Artifacts] r) => Sem (Reader FunctionsTable ': r) a -> Sem r a
readerFunctionsTableArtifacts = runReaderArtifacts (artifactTypeCheckingTables . typeCheckingTablesFunctionsTable)

readerBuiltinsTableArtifacts :: (Members '[State Artifacts] r) => Sem (Reader BuiltinsTable ': r) a -> Sem r a
readerBuiltinsTableArtifacts = runReaderArtifacts (artifactScopeTable . infoBuiltins)

readerTypesTableArtifacts :: (Members '[State Artifacts] r) => Sem (Reader TypesTable ': r) a -> Sem r a
readerTypesTableArtifacts = runReaderArtifacts (artifactTypeCheckingTables . typeCheckingTablesTypesTable)

runTerminationArtifacts :: (Members '[Error JuvixError, State Artifacts] r) => Sem (Termination ': r) a -> Sem r a
runTerminationArtifacts = runStateLikeArtifacts runTermination artifactTerminationState

runStateArtifacts :: (Members '[State Artifacts] r) => Lens' Artifacts f -> Sem (State f ': r) a -> Sem r a
runStateArtifacts = runStateLikeArtifacts runState

runReaderArtifacts :: (Members '[State Artifacts] r) => Lens' Artifacts f -> Sem (Reader f ': r) a -> Sem r a
runReaderArtifacts l m = do
  s <- gets (^. l)
  runReader s m

runStateLikeArtifacts ::
  (Members '[State Artifacts] r) =>
  (field -> Sem (stateEff ': r) a -> Sem r (field, a)) ->
  Lens' Artifacts field ->
  Sem (stateEff ': r) a ->
  Sem r a
runStateLikeArtifacts runner l m = do
  s <- gets (^. l)
  (s', a) <- runner s m
  modify' (set l s')
  return a

runResultBuilderArtifacts :: forall r a. (Members '[State Artifacts] r) => Sem (ResultBuilder ': r) a -> Sem r a
runResultBuilderArtifacts m = do
  tabs <- gets (^. artifactTypeCheckingTables)
  let importCtx = ImportContext tabs
  (s, a) <- runResultBuilder importCtx m
  modify' (set artifactTypeCheckingTables (s ^. resultBuilderStateCombinedTables))
  return a
