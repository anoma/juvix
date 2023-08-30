module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error.Pretty,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error.Types,
  )
where

import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error.Types
import Juvix.Prelude

data TraitError
  = ErrNotATrait NotATrait
  | ErrNoInstance NoInstance
  | ErrAmbiguousInstances AmbiguousInstances
  | ErrExplicitInstanceArgument ExplicitInstanceArgument

instance ToGenericError TraitError where
  genericError :: (Member (Reader GenericOptions) r) => TraitError -> Sem r GenericError
  genericError = \case
    ErrNotATrait e -> genericError e
    ErrNoInstance e -> genericError e
    ErrAmbiguousInstances e -> genericError e
    ErrExplicitInstanceArgument e -> genericError e
