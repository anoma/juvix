module Juvix.Syntax.MicroJuvix.TypeChecker.FunctionsTable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language.Extra

newtype FunctionsTable = FunctionsTable
  { _functionsTable :: HashMap FunctionName Expression
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''FunctionsTable

registerFunctionDef :: Member (State FunctionsTable) r => FunctionDef -> Sem r ()
registerFunctionDef f = whenJust (functionDefEval f) $ \e ->
  modify (over functionsTable (HashMap.insert (f ^. funDefName) e))

askFunctionDef :: Member (Reader FunctionsTable) r => FunctionName -> Sem r (Maybe Expression)
askFunctionDef f = asks (^. functionsTable . at f)
