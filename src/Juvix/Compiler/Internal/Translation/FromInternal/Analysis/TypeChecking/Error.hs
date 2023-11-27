module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Types,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error,
  )
where

import Juvix.Compiler.Builtins.Error (NotDefined)
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
  | ErrBuiltinNotDefined NotDefined
  | ErrArityCheckerError ArityCheckerError
  | ErrDefaultArgLoop DefaultArgLoop

instance ToGenericError TypeCheckerError where
  genericError :: (Member (Reader GenericOptions) r) => TypeCheckerError -> Sem r GenericError
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
    ErrBuiltinNotDefined e -> genericError e
    ErrArityCheckerError e -> genericError e
    ErrDefaultArgLoop e -> genericError e
