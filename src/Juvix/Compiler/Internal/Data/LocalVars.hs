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

emptyLocalVars :: LocalVars
emptyLocalVars =
  LocalVars
    { _localTypes = mempty,
      _localTyMap = mempty
    }

instance Monoid LocalVars where
  mempty = emptyLocalVars

makeLenses ''LocalVars

withLocalTypeMaybe :: (Members '[Reader LocalVars] r) => Maybe VarName -> Expression -> Sem r a -> Sem r a
withLocalTypeMaybe = \case
  Nothing -> const id
  Just n -> withLocalType n

withLocalTypes :: (Members '[Reader LocalVars] r) => [(VarName, Expression)] -> Sem r a -> Sem r a
withLocalTypes assocs = local (over localTypes (<> HashMap.fromList assocs))

withLocalType :: (Members '[Reader LocalVars] r) => VarName -> Expression -> Sem r a -> Sem r a
withLocalType v ty = withLocalTypes [(v, ty)]

addType :: VarName -> Expression -> LocalVars -> LocalVars
addType v t = over localTypes (HashMap.insert v t)

addTypeMapping :: VarName -> VarName -> LocalVars -> LocalVars
addTypeMapping v v' = over localTyMap (HashMap.insert v v')

withEmptyLocalVars :: Sem (Reader LocalVars ': r) a -> Sem r a
withEmptyLocalVars = runReader emptyLocalVars

withLocalVars :: LocalVars -> Sem (Reader LocalVars ': r) a -> Sem r a
withLocalVars = runReader
