module Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments.Error where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Prelude

data NamedArgumentsError
  = ErrDuplicateArgument DuplicateArgument
  | ErrUnexpectedArguments UnexpectedArguments
  | ErrMissingArguments MissingArguments
  deriving stock (Show)

instance ToGenericError NamedArgumentsError where
  genericError = \case
    ErrDuplicateArgument d -> genericError d
    ErrUnexpectedArguments d -> genericError d
    ErrMissingArguments d -> genericError d

newtype DuplicateArgument = DuplicateArgument
  { _duplicateArguments :: NonEmpty Symbol
  }
  deriving stock (Show)

instance ToGenericError DuplicateArgument where
  genericError DuplicateArgument {..} = do
    opts <- fromGenericOptions <$> ask
    let _genericErrorLoc = getLocSpan _duplicateArguments
        _genericErrorMessage :: AnsiText
        argumentStr = plural "argument" "arguments" (length _duplicateArguments)
        _genericErrorMessage =
          prettyError $
            "Duplicate"
              <+> argumentStr
              <+> "in named application:"
              <+> hsep (ppCode opts <$> _duplicateArguments)
        _genericErrorIntervals = getLoc <$> toList _duplicateArguments
    return GenericError {..}

newtype UnexpectedArguments = UnexpectedArguments
  { _unexpectedArguments :: NonEmpty (NamedArgument 'Scoped)
  }
  deriving stock (Show)

instance ToGenericError UnexpectedArguments where
  genericError UnexpectedArguments {..} = do
    opts <- fromGenericOptions <$> ask
    let _genericErrorLoc = getLocSpan _unexpectedArguments
        _genericErrorMessage :: AnsiText
        _genericErrorMessage =
          prettyError $
            "Unexpected named arguments:"
              <> line
              <> itemize (ppCode opts <$> _unexpectedArguments)
        _genericErrorIntervals = getLoc <$> toList _unexpectedArguments
    return GenericError {..}

data MissingArguments = MissingArguments
  { _missingArgumentsLoc :: Interval,
    _missingArguments :: NonEmpty Symbol
  }
  deriving stock (Show)

instance ToGenericError MissingArguments where
  genericError MissingArguments {..} = do
    opts <- fromGenericOptions <$> ask
    let i = _missingArgumentsLoc
        _genericErrorLoc = i
        _genericErrorMessage :: AnsiText
        _genericErrorMessage =
          prettyError $
            "Missing arguments in named application:"
              <> line
              <> itemize (ppCode opts <$> _missingArguments)
        _genericErrorIntervals = pure i
    return GenericError {..}
