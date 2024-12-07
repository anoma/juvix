module Anoma.Effect.Intents.Verify where

import Anoma.Effect.Base
import Anoma.Rpc.Intents.Verify
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value)
import Juvix.Prelude.Aeson qualified as Aeson

newtype VerifyInput = VerifyInput
  {_verifyIntent :: Nockma.Term Natural}

newtype VerifyResult = VerifyResult
  {_verifyResultValid :: Bool}

makeLenses ''VerifyInput
makeLenses ''VerifyResult

verify ::
  forall r.
  (Members '[Anoma, Error SimpleError, Logger] r) =>
  VerifyInput ->
  Sem r VerifyResult
verify i = do
  let intent = encodeJam64 (i ^. verifyIntent)
  nodeInfo <- getNodeInfo
  let msg = Request {_requestNodeInfo = nodeInfo, _requestIntent = Intent {_intentIntent = intent}}
  logMessageValue "Request payload" msg
  resVal :: Value <- anomaRpc verifyGrpcUrl (Aeson.toJSON msg) >>= fromJSONErr
  logMessageValue "Response Payload" resVal
  res :: Response <- fromJSONErr resVal
  return VerifyResult {_verifyResultValid = res ^. responseValid}
