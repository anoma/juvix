module Juvix.Compiler.Builtins.Error
  ( module Juvix.Compiler.Builtins.Error,
    module Juvix.Compiler.Builtins.Error.Types,
  )
where

import Juvix.Compiler.Builtins.Error.Types
import Juvix.Prelude
import Juvix.Prelude.Pretty

data BuiltinsError
  = ErrAlreadyDefined AlreadyDefined
  | ErrNotDefined NotDefined
  | ErrBuiltinsErrorMessage BuiltinsErrorMessage

instance ToGenericError BuiltinsError where
  genericError = \case
    ErrAlreadyDefined e -> genericError e
    ErrNotDefined e -> genericError e
    ErrBuiltinsErrorMessage e -> genericError e

builtinsErrorMsg :: (Members '[Error BuiltinsError] r) => Interval -> AnsiText -> Sem r a
builtinsErrorMsg loc msg =
  throw $
    ErrBuiltinsErrorMessage
      BuiltinsErrorMessage
        { _builtinsErrorMessageLoc = loc,
          _builtinsErrorMessage = msg
        }

builtinsErrorText :: (Members '[Error BuiltinsError] r) => Interval -> Text -> Sem r a
builtinsErrorText loc = builtinsErrorMsg loc . mkAnsiText
