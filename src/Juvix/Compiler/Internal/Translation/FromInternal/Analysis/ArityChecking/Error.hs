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
  genericError :: (Member (Reader GenericOptions) r) => ArityCheckerError -> Sem r GenericError
  genericError = \case
    ErrWrongConstructorAppLength e -> genericError e
    ErrLhsTooManyPatterns e -> genericError e
    ErrWrongPatternIsImplicit e -> genericError e
    ErrExpectedExplicitArgument e -> genericError e
    ErrPatternFunction e -> genericError e
    ErrTooManyArguments e -> genericError e
    ErrFunctionApplied e -> genericError e
