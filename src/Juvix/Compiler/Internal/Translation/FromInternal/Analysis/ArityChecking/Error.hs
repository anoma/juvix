module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error.Types,
  )
where

import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error.Types
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
  genericError :: GenericOptions -> ArityCheckerError -> GenericError
  genericError opts = \case
    ErrWrongConstructorAppLength e -> genericError opts e
    ErrLhsTooManyPatterns e -> genericError opts e
    ErrWrongPatternIsImplicit e -> genericError opts e
    ErrExpectedExplicitArgument e -> genericError opts e
    ErrPatternFunction e -> genericError opts e
    ErrTooManyArguments e -> genericError opts e
    ErrFunctionApplied e -> genericError opts e
