module Juvix.Compiler.Store.Internal.Data.FunctionsTable where

import Juvix.Compiler.Internal.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

newtype FunctionsTable = FunctionsTable
  { _functionsTable :: HashMap FunctionName Expression
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Generic)

instance Serialize FunctionsTable

makeLenses ''FunctionsTable

askFunctionDef :: (Member (State FunctionsTable) r) => FunctionName -> Sem r (Maybe Expression)
askFunctionDef f = gets (^. functionsTable . at f)
