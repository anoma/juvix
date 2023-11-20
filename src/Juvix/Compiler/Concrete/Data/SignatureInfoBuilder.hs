module Juvix.Compiler.Concrete.Data.SignatureInfoBuilder
  ( module Juvix.Compiler.Concrete.Data.SignatureInfoBuilder,
    module Juvix.Compiler.Store.Scoped.Data.SignatureInfo,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Store.Scoped.Data.SignatureInfo
import Juvix.Prelude

data SignatureInfoBuilder m a where
  RegisterConstructorSig :: NameId -> RecordNameSignature 'Scoped -> SignatureInfoBuilder m ()
  RegisterNameSig :: NameId -> NameSignature 'Scoped -> SignatureInfoBuilder m ()

makeSem ''SignatureInfoBuilder

toState :: Sem (SignatureInfoBuilder ': r) a -> Sem (State SignatureInfo ': r) a
toState = reinterpret $ \case
  RegisterConstructorSig n sig ->
    modify (over sigInfoConstructorSigs (HashMap.insert n sig))
  RegisterNameSig n sig ->
    modify (over sigInfoNameSigs (HashMap.insert n sig))

runSignatureInfoBuilder :: Sem (SignatureInfoBuilder ': r) a -> Sem r (SignatureInfo, a)
runSignatureInfoBuilder = runState mempty . toState

ignoreSignatureInfoBuilder :: Sem (SignatureInfoBuilder ': r) a -> Sem r a
ignoreSignatureInfoBuilder = evalState mempty . toState
