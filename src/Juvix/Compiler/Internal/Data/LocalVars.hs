module Juvix.Compiler.Internal.Data.LocalVars where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

newtype LocalVars = LocalVars
  { _localTypes :: HashMap VarName Expression
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''LocalVars

withLocalTypeMaybe :: (Members '[Reader LocalVars] r) => Maybe VarName -> Expression -> Sem r a -> Sem r a
withLocalTypeMaybe = \case
  Nothing -> const id
  Just n -> withLocalType n

withLocalType :: (Members '[Reader LocalVars] r) => VarName -> Expression -> Sem r a -> Sem r a
withLocalType v ty = local (over localTypes (HashMap.insert v ty))

addType :: VarName -> Expression -> LocalVars -> LocalVars
addType v t = over localTypes (HashMap.insert v t)
