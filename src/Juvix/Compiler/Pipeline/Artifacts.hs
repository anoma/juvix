-- | Stuff that is generated when the pipeline is run
module Juvix.Compiler.Pipeline.Artifacts where

import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Prelude

data Artifacts = Artifacts
  { _artifactBuiltins :: BuiltinsState,
    _artifactResolver :: ResolverState
  }

makeLenses ''Artifacts
