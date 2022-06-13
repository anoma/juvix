module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error
  ( module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error,
    module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error.Types,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error.Types

data ArityCheckerError
  = ErrWrongConstructorAppLength WrongConstructorAppLength
  | ErrLhsTooManyPatterns LhsTooManyPatterns
  | ErrExpectedExplicitPattern ExpectedExplicitPattern
  | ErrExpectedExplicitArgument ExpectedExplicitArgument
  | ErrPatternFunction PatternFunction
  | ErrTooManyArguments TooManyArguments
  | ErrFunctionApplied FunctionApplied

instance ToGenericError ArityCheckerError where
  genericError :: ArityCheckerError -> GenericError
  genericError = \case
    ErrWrongConstructorAppLength e -> genericError e
    ErrLhsTooManyPatterns e -> genericError e
    ErrExpectedExplicitPattern e -> genericError e
    ErrExpectedExplicitArgument e -> genericError e
    ErrPatternFunction e -> genericError e
    ErrTooManyArguments e -> genericError e
    ErrFunctionApplied e -> genericError e
