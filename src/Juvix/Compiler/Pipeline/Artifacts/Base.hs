module Juvix.Compiler.Pipeline.Artifacts.Base where

import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoped
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder (BuilderState)
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.Scope qualified as Scoped
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Data
import Juvix.Compiler.Core.Data.InfoTableBuilder qualified as Core
import Juvix.Compiler.Internal.Translation.FromConcrete qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Prelude

-- | `Artifacts` contains enough information so that the pipeline can be
-- restarted while preserving existing state.
data Artifacts = Artifacts
  { _artifactParsing :: BuilderState,
    -- Scoping
    _artifactResolver :: ResolverState,
    _artifactBuiltins :: BuiltinsState,
    _artifactNameIdState :: Stream NameId,
    _artifactScopeTable :: Scoped.InfoTable,
    _artifactScopeExports :: HashSet NameId,
    _artifactMainModuleScope :: Maybe Scope,
    _artifactScoperState :: Scoped.ScoperState,
    -- Concrete -> Internal
    _artifactInternalModuleCache :: Internal.ModulesCache,
    _artifactTerminationState :: TerminationState,
    -- Typechecking
    _artifactTypes :: TypesTable,
    _artifactFunctions :: FunctionsTable,
    -- | This includes the InfoTable from all type checked modules
    _artifactInternalTypedTable :: Internal.InfoTable,
    -- Core
    _artifactCoreTable :: Core.InfoTable
  }

makeLenses ''Artifacts
