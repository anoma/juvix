module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Error
 (module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Error,
  module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Error.Types,
 )

where

import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Error.Types
import Juvix.Prelude

data NonStrictlyPositiveError
  = ErrTypeInNegativePosition TypeInNegativePosition
  | ErrTypeAsArgumentOfBoundVar TypeAsArgumentOfBoundVar

instance ToGenericError NonStrictlyPositiveError where
  genericError :: (Member (Reader GenericOptions) r) => NonStrictlyPositiveError -> Sem r GenericError
  genericError = \case
    ErrTypeInNegativePosition e -> genericError e
    ErrTypeAsArgumentOfBoundVar e -> genericError e

