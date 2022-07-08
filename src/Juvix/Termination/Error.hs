module Juvix.Termination.Error
  ( module Juvix.Termination.Error,
    module Juvix.Termination.Error.Pretty,
    module Juvix.Termination.Error.Types,
  )
where

import Juvix.Prelude
import Juvix.Termination.Error.Pretty
import Juvix.Termination.Error.Types

newtype TerminationError
  = ErrNoLexOrder NoLexOrder
  deriving stock (Show)

instance ToGenericError TerminationError where
  genericError :: TerminationError -> GenericError
  genericError = \case
    ErrNoLexOrder e -> genericError e
