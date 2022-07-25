module Juvix.Syntax.MicroJuvix.Error
  ( module Juvix.Syntax.MicroJuvix.Error,
    module Juvix.Syntax.MicroJuvix.Error.Pretty,
    module Juvix.Syntax.MicroJuvix.Error.Types,
    module Juvix.Analysis.Arity.Error,
  )
where

import Juvix.Analysis.Arity.Error
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Error.Pretty
import Juvix.Syntax.MicroJuvix.Error.Types

data TypeCheckerError
  = ErrWrongConstructorType WrongConstructorType
  | ErrWrongReturnType WrongReturnType
  | ErrArity ArityCheckerError
  | ErrWrongType WrongType
  | ErrUnsolvedMeta UnsolvedMeta
  | ErrExpectedFunctionType ExpectedFunctionType
  | ErrTooManyArgumentsIndType WrongNumberArgumentsIndType
  | ErrTooFewArgumentsIndType WrongNumberArgumentsIndType
  | ErrImpracticalPatternMatching ImpracticalPatternMatching
  | ErrNoPositivity NoPositivity

instance ToGenericError TypeCheckerError where
  genericError :: TypeCheckerError -> GenericError
  genericError = \case
    ErrWrongConstructorType e -> genericError e
    ErrWrongReturnType e -> genericError e
    ErrArity e -> genericError e
    ErrWrongType e -> genericError e
    ErrUnsolvedMeta e -> genericError e
    ErrExpectedFunctionType e -> genericError e
    ErrTooManyArgumentsIndType e -> genericError e
    ErrTooFewArgumentsIndType e -> genericError e
    ErrImpracticalPatternMatching e -> genericError e
    ErrNoPositivity e -> genericError e
