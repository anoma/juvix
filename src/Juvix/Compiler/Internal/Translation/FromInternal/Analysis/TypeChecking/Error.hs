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
  | ErrUnsupportedTypeFunction UnsupportedTypeFunction
  | ErrTargetNotATrait TargetNotATrait
  | ErrNotATrait NotATrait
  | ErrNoInstance NoInstance
  | ErrAmbiguousInstances AmbiguousInstances
  | ErrExplicitInstanceArgument ExplicitInstanceArgument
  | ErrTraitNotTerminating TraitNotTerminating

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
    ErrTargetNotATrait e -> genericError e
    ErrNotATrait e -> genericError e
    ErrNoInstance e -> genericError e
    ErrAmbiguousInstances e -> genericError e
    ErrExplicitInstanceArgument e -> genericError e
    ErrTraitNotTerminating e -> genericError e
