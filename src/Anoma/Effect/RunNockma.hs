module Anoma.Effect.RunNockma
  ( module Anoma.Effect.RunNockma,
    module Anoma.Rpc.RunNock,
  )
where

import Anoma.Effect.Base
import Anoma.Rpc.RunNock
import Juvix.Compiler.Nockma.Encoding (decodeCue64, encodeJam64)
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value)
import Juvix.Prelude.Aeson qualified as Aeson
import Juvix.Prelude.Pretty

data RunNockmaInput = RunNockmaInput
  { _runNockmaProgram :: AnomaResult,
    _runNockmaInput :: [Nockma.Term Natural]
  }

makeLenses ''RunNockmaInput

decodeJam64 :: (Members '[Error SimpleError] r) => Text -> Sem r (Nockma.Term Natural)
decodeJam64 encoded =
  case Base64.decode (encodeUtf8 encoded) of
    Left err -> throw (SimpleError (mkAnsiText err))
    Right bs' ->
      case cueFromByteString'' bs' of
        Left (err :: NockNaturalNaturalError) -> throw (simpleErrorCodeAnn err)
        Right (Left (err :: DecodingError)) -> throw (simpleErrorCodeAnn err)
        Right (Right r) -> return r

encodeJam64 :: Nockma.Term Natural -> Text
encodeJam64 = decodeUtf8 . Base64.encode . jamToByteString

fromJSON :: (Members '[Error SimpleError, Logger] r) => (Aeson.FromJSON a) => Value -> Sem r a
fromJSON v = case Aeson.fromJSON v of
  Aeson.Success r -> return r
  Aeson.Error err -> throw (SimpleError (mkAnsiText err))

runNockma ::
  (Members '[Anoma, Error SimpleError, Logger] r) =>
  Nockma.Term Natural ->
  [Nockma.Term Natural] ->
  Sem r (Nockma.Term Natural)
runNockma prog inputs = do
  let prog' = encodeJam64 prog
      args = map (NockInputJammed . encodeJam64) inputs
      msg =
        RunNock
          { _runNockJammedProgram = prog',
            _runNockPrivateInputs = args,
            _runNockPublicInputs = []
          }
  logVerbose (mkAnsiText ("Request Payload:\n" <> Aeson.jsonEncodeToPrettyText msg))
  res :: Response <- anomaRpc runNockGrpcUrl (Aeson.toJSON msg) >>= fromJSON
  logVerbose (mkAnsiText ("Response Payload:\n" <> Aeson.jsonEncodeToPrettyText res))
  case res of
    ResponseProof x -> decodeCue64 x
    ResponseError err -> throw (SimpleError (mkAnsiText err))
