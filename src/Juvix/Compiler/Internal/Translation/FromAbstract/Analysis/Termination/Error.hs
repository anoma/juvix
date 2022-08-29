module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error,
    module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Types,
  )
where

import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Types
import Juvix.Prelude

newtype TerminationError
  = ErrNoLexOrder NoLexOrder
  deriving stock (Show)

instance ToGenericError TerminationError where
  genericError :: GenericOptions -> TerminationError -> GenericError
  genericError opts = \case
    ErrNoLexOrder e -> genericError opts e
