module Juvix.Analysis.TypeChecking.FunctionsTable where

import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language

newtype FunctionsTable = FunctionsTable
  { _functionsTable :: HashMap FunctionName Expression
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''FunctionsTable

askFunctionDef :: Member (Reader FunctionsTable) r => FunctionName -> Sem r (Maybe Expression)
askFunctionDef f = asks (^. functionsTable . at f)
