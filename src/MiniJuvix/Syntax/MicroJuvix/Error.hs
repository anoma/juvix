module MiniJuvix.Syntax.MicroJuvix.Error
  ( module MiniJuvix.Syntax.MicroJuvix.Error,
    module MiniJuvix.Syntax.MicroJuvix.Error.Pretty,
    module MiniJuvix.Syntax.MicroJuvix.Error.Types,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty
import MiniJuvix.Syntax.MicroJuvix.Error.Types

data TypeCheckerError
  = ErrTooManyPatterns TooManyPatterns
  | ErrWrongConstructorType WrongConstructorType
  | ErrWrongConstructorAppArgs WrongConstructorAppArgs
  | ErrWrongType WrongType
  | ErrExpectedFunctionType ExpectedFunctionType
  deriving stock (Show)

instance ToGenericError TypeCheckerError where
  genericError :: TypeCheckerError -> GenericError
  genericError = \case
    ErrTooManyPatterns e -> genericError e
    ErrWrongConstructorType e -> genericError e
    ErrWrongConstructorAppArgs e -> genericError e
    ErrWrongType e -> genericError e
    ErrExpectedFunctionType e -> genericError e
