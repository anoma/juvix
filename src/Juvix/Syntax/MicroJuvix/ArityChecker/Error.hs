module Juvix.Syntax.MicroJuvix.ArityChecker.Error
  ( module Juvix.Syntax.MicroJuvix.ArityChecker.Error,
    module Juvix.Syntax.MicroJuvix.ArityChecker.Error.Types,
  )
where

import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.ArityChecker.Error.Types

data ArityCheckerError
  = ErrWrongConstructorAppLength WrongConstructorAppLength
  | ErrLhsTooManyPatterns LhsTooManyPatterns
  | ErrWrongPatternIsImplicit WrongPatternIsImplicit
  | ErrExpectedExplicitArgument ExpectedExplicitArgument
  | ErrPatternFunction PatternFunction
  | ErrTooManyArguments TooManyArguments
  | ErrFunctionApplied FunctionApplied

instance ToGenericError ArityCheckerError where
  genericError :: ArityCheckerError -> GenericError
  genericError = \case
    ErrWrongConstructorAppLength e -> genericError e
    ErrLhsTooManyPatterns e -> genericError e
    ErrWrongPatternIsImplicit e -> genericError e
    ErrExpectedExplicitArgument e -> genericError e
    ErrPatternFunction e -> genericError e
    ErrTooManyArguments e -> genericError e
    ErrFunctionApplied e -> genericError e
