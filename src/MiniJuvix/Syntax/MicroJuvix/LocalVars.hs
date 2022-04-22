module MiniJuvix.Syntax.MicroJuvix.LocalVars where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

data LocalVars = LocalVars
  { _localTypes :: HashMap VarName Type,
    _localTyMap :: HashMap VarName VarName
  }
  deriving stock (Show)

makeLenses ''LocalVars

addType :: VarName -> Type -> LocalVars -> LocalVars
addType v t = over localTypes (HashMap.insert v t)

emptyLocalVars :: LocalVars
emptyLocalVars =
  LocalVars
    { _localTypes = mempty,
      _localTyMap = mempty
    }
