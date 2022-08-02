module Juvix.Compiler.Internal.Data.LocalVars where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data LocalVars = LocalVars
  { _localTypes :: HashMap VarName Expression,
    _localTyMap :: HashMap VarName VarName
  }

makeLenses ''LocalVars

addType :: VarName -> Expression -> LocalVars -> LocalVars
addType v t = over localTypes (HashMap.insert v t)

emptyLocalVars :: LocalVars
emptyLocalVars =
  LocalVars
    { _localTypes = mempty,
      _localTyMap = mempty
    }
