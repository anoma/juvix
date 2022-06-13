module MiniJuvix.Syntax.MicroJuvix.Error
  ( module MiniJuvix.Syntax.MicroJuvix.Error,
    module MiniJuvix.Syntax.MicroJuvix.Error.Pretty,
    module MiniJuvix.Syntax.MicroJuvix.Error.Types,
    module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty
import MiniJuvix.Syntax.MicroJuvix.Error.Types

data TypeCheckerError
  = ErrWrongConstructorType WrongConstructorType
  | ErrArity ArityCheckerError
  | ErrWrongType WrongType
  | ErrUnsolvedMeta UnsolvedMeta
  | ErrExpectedFunctionType ExpectedFunctionType

instance ToGenericError TypeCheckerError where
  genericError :: TypeCheckerError -> GenericError
  genericError = \case
    ErrWrongConstructorType e -> genericError e
    ErrArity e -> genericError e
    ErrWrongType e -> genericError e
    ErrUnsolvedMeta e -> genericError e
    ErrExpectedFunctionType e -> genericError e
