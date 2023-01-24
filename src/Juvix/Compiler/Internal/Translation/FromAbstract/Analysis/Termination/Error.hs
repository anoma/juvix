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
  genericError :: (Member (Reader GenericOptions) r) => TerminationError -> Sem r GenericError
  genericError = \case
    ErrNoLexOrder e -> genericError e
