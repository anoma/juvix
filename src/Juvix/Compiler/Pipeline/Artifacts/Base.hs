module Juvix.Compiler.Pipeline.Artifacts.Base where

import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.Scope qualified as Scoped
import Juvix.Compiler.Concrete.Translation.FromSource.Data.ParserState
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Pipeline.Loader.PathResolver.Data
import Juvix.Compiler.Store.Internal.Data.CoercionInfo
import Juvix.Compiler.Store.Internal.Data.InstanceInfo
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude

-- | `Artifacts` contains enough information so that the pipeline can be
-- restarted while preserving existing state.
data Artifacts = Artifacts
  { _artifactParsing :: ParserState,
    -- Scoping
    _artifactResolver :: ResolverState,
    _artifactBuiltins :: BuiltinsState,
    _artifactNameIdState :: NameIdGenState,
    _artifactScopeTable :: Scoped.InfoTable,
    _artifactScopeExports :: HashSet NameId,
    _artifactMainModuleScope :: Maybe Scope,
    _artifactScoperState :: Scoped.ScoperState,
    -- Concrete -> Internal
    _artifactTerminationState :: TerminationState,
    -- Typechecking
    _artifactTypes :: TypesTable,
    _artifactFunctions :: FunctionsTable,
    _artifactInstances :: InstanceTable,
    _artifactCoercions :: CoercionTable,
    -- | This includes the InfoTable from all type checked modules
    _artifactInternalTypedTable :: Internal.InfoTable,
    -- Core
    _artifactCoreModule :: Core.Module,
    -- Store
    _artifactModuleTable :: Store.ModuleTable
  }

makeLenses ''Artifacts
