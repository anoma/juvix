module Juvix.Analysis.Termination.Error
  ( module Juvix.Analysis.Termination.Error,
    module Juvix.Analysis.Termination.Error.Pretty,
    module Juvix.Analysis.Termination.Error.Types,
  )
where

import Juvix.Analysis.Termination.Error.Pretty
import Juvix.Analysis.Termination.Error.Types
import Juvix.Prelude

newtype TerminationError
  = ErrNoLexOrder NoLexOrder
  deriving stock (Show)

instance ToGenericError TerminationError where
  genericError :: TerminationError -> GenericError
  genericError = \case
    ErrNoLexOrder e -> genericError e
