module Juvix.Compiler.Nockma.Evaluator.Error where

import Juvix.Prelude hiding (Atom)
import Juvix.Prelude.Pretty

data NockEvalError
  = InvalidPath
  | ExpectedAtom
  | ExpectedCell Text
  | NoStack
  | AssignmentNotFound Text
  deriving stock (Show)

newtype GenericNockEvalError = GenericNockEvalError
  { _genericNockEvalErrorMessage :: AnsiText
  }

class ToGenericNockEvalError a where
  toGenericNockEvalError :: a -> GenericNockEvalError
