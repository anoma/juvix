module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Types,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error,
  )
where

import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Types
import Juvix.Prelude
import Prelude (show)

data TypeCheckerError
  = ErrWrongConstructorType WrongConstructorType
  | ErrWrongReturnType WrongReturnType
  | ErrWrongType WrongType
  | ErrUnsolvedMeta UnsolvedMeta
  | ErrExpectedFunctionType ExpectedFunctionType
  | ErrTooManyArgumentsIndType WrongNumberArgumentsIndType
  | ErrTooFewArgumentsIndType WrongNumberArgumentsIndType
  | ErrInvalidPatternMatching InvalidPatternMatching
  | ErrNonStrictlyPositive NonStrictlyPositiveError
  | ErrUnsupportedTypeFunction UnsupportedTypeFunction
  | ErrInvalidInstanceType InvalidInstanceType
  | ErrInvalidCoercionType InvalidCoercionType
  | ErrWrongCoercionArgument WrongCoercionArgument
  | ErrCoercionCycles CoercionCycles
  | ErrTargetNotATrait TargetNotATrait
  | ErrNotATrait NotATrait
  | ErrNoInstance NoInstance
  | ErrAmbiguousInstances AmbiguousInstances
  | ErrSubsumedInstance SubsumedInstance
  | ErrExplicitInstanceArgument ExplicitInstanceArgument
  | ErrTraitNotTerminating TraitNotTerminating
  | ErrArityCheckerError ArityCheckerError
  | ErrDefaultArgLoop DefaultArgLoop

instance ToGenericError TypeCheckerError where
  genericError :: (Member (Reader GenericOptions) r) => TypeCheckerError -> Sem r GenericError
  genericError = \case
    ErrWrongConstructorType e -> genericError e
    ErrWrongReturnType e -> genericError e
    ErrWrongType e -> genericError e
    ErrUnsolvedMeta e -> genericError e
    ErrExpectedFunctionType e -> genericError e
    ErrTooManyArgumentsIndType e -> genericError e
    ErrTooFewArgumentsIndType e -> genericError e
    ErrInvalidPatternMatching e -> genericError e
    ErrNonStrictlyPositive e -> genericError e
    ErrUnsupportedTypeFunction e -> genericError e
    ErrInvalidInstanceType e -> genericError e
    ErrInvalidCoercionType e -> genericError e
    ErrWrongCoercionArgument e -> genericError e
    ErrCoercionCycles e -> genericError e
    ErrTargetNotATrait e -> genericError e
    ErrNotATrait e -> genericError e
    ErrNoInstance e -> genericError e
    ErrAmbiguousInstances e -> genericError e
    ErrSubsumedInstance e -> genericError e
    ErrExplicitInstanceArgument e -> genericError e
    ErrTraitNotTerminating e -> genericError e
    ErrArityCheckerError e -> genericError e
    ErrDefaultArgLoop e -> genericError e

instance Show TypeCheckerError where
  show = \case
    ErrWrongConstructorType {} -> "ErrWrongConstructorType"
    ErrWrongReturnType {} -> "ErrWrongReturnType"
    ErrWrongType {} -> "ErrWrongType"
    ErrUnsolvedMeta {} -> "ErrUnsolvedMeta"
    ErrExpectedFunctionType {} -> "ErrExpectedFunctionType"
    ErrTooManyArgumentsIndType {} -> "ErrTooManyArgumentsIndType"
    ErrTooFewArgumentsIndType {} -> "ErrTooFewArgumentsIndType"
    ErrInvalidPatternMatching {} -> "ErrInvalidPatternMatching"
    ErrNoPositivity {} -> "ErrNoPositivity"
    ErrUnsupportedTypeFunction {} -> "ErrUnsupportedTypeFunction"
    ErrInvalidInstanceType {} -> "ErrInvalidInstanceType"
    ErrInvalidCoercionType {} -> "ErrInvalidCoercionType"
    ErrWrongCoercionArgument {} -> "ErrWrongCoercionArgument"
    ErrCoercionCycles {} -> "ErrCoercionCycles"
    ErrTargetNotATrait {} -> "ErrTargetNotATrait"
    ErrNotATrait {} -> "ErrNotATrait"
    ErrNoInstance {} -> "ErrNoInstance"
    ErrAmbiguousInstances {} -> "ErrAmbiguousInstances"
    ErrSubsumedInstance {} -> "ErrSubsumedInstance"
    ErrExplicitInstanceArgument {} -> "ErrExplicitInstanceArgument"
    ErrTraitNotTerminating {} -> "ErrTraitNotTerminating"
    ErrArityCheckerError {} -> "ErrArityCheckerError"
    ErrDefaultArgLoop {} -> "ErrDefaultArgLoop"
