module Juvix.Compiler.Nockma.Evaluator.Error
  ( module Juvix.Compiler.Nockma.Evaluator.Error,
    module Juvix.Compiler.Nockma.Evaluator.Crumbs,
  )
where

import Juvix.Compiler.Nockma.Evaluator.Crumbs
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Prelude hiding (Atom)
import Juvix.Prelude.Pretty

data NockEvalError
  = InvalidPath EvalCtx
  | ExpectedAtom
  | ExpectedCell Text
  | NoStack
  | AssignmentNotFound Text

newtype GenericNockEvalError = GenericNockEvalError
  { _genericNockEvalErrorMessage :: AnsiText
  }

class ToGenericNockEvalError a where
  toGenericNockEvalError :: a -> GenericNockEvalError

instance PrettyCode NockEvalError where
  ppCode = \case
    InvalidPath ctx -> ppCode ctx
