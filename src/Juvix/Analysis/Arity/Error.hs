module Juvix.Analysis.Arity.Error
  ( module Juvix.Analysis.Arity.Error,
    module Juvix.Analysis.Arity.Error.Types,
  )
where

import Juvix.Analysis.Arity.Error.Types
import Juvix.Prelude

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
