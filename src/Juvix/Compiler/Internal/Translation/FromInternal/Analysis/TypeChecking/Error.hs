module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Types,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error,
  )
where

import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Types
import Juvix.Prelude

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
  genericError :: GenericOptions -> TypeCheckerError -> GenericError
  genericError opts = \case
    ErrWrongConstructorType e -> genericError opts e
    ErrWrongReturnType e -> genericError opts e
    ErrArity e -> genericError opts e
    ErrWrongType e -> genericError opts e
    ErrUnsolvedMeta e -> genericError opts e
    ErrExpectedFunctionType e -> genericError opts e
    ErrTooManyArgumentsIndType e -> genericError opts e
    ErrTooFewArgumentsIndType e -> genericError opts e
    ErrImpracticalPatternMatching e -> genericError opts e
    ErrNoPositivity e -> genericError opts e
