module Juvix.Compiler.Store.Scoped.Data.SignatureInfo where

import Juvix.Compiler.Concrete.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

type NameSignatures = HashMap NameId (NameSignature 'Scoped)

data SignatureInfo = SignatureInfo
  { _sigInfoConstructorSigs :: HashMap NameId (RecordNameSignature 'Scoped),
    _sigInfoFunctionSigs :: HashMap NameId (NameSignature 'Scoped)
  }
  deriving stock (Generic)

instance Serialize SignatureInfo

makeLenses ''SignatureInfo

instance Semigroup SignatureInfo where
  s1 <> s2 =
    SignatureInfo
      { _sigInfoConstructorSigs = s1 ^. sigInfoConstructorSigs <> s2 ^. sigInfoConstructorSigs,
        _sigInfoFunctionSigs = s1 ^. sigInfoFunctionSigs <> s2 ^. sigInfoFunctionSigs
      }

instance Monoid SignatureInfo where
  mempty = SignatureInfo mempty mempty
