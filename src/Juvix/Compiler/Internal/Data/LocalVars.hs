module Juvix.Compiler.Internal.Data.LocalVars where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data LocalVars = LocalVars
  { _localTypes :: HashMap VarName Expression,
    _localTyMap :: HashMap VarName VarName
  }

instance Semigroup LocalVars where
  (LocalVars a b) <> (LocalVars a' b') = LocalVars (a <> a') (b <> b')

makeLenses ''LocalVars

addType :: VarName -> Expression -> LocalVars -> LocalVars
addType v t = over localTypes (HashMap.insert v t)

addTypeMapping :: VarName -> VarName -> LocalVars -> LocalVars
addTypeMapping v v' = over localTyMap (HashMap.insert v v')

emptyLocalVars :: LocalVars
emptyLocalVars =
  LocalVars
    { _localTypes = mempty,
      _localTyMap = mempty
    }
