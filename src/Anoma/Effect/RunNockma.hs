module Anoma.Effect.RunNockma
  ( module Anoma.Effect.RunNockma,
    module Anoma.Rpc.RunNock,
  )
where

import Anoma.Effect.Base
import Anoma.Rpc.RunNock (RunNock (_runNockJammedProgram))
import Anoma.Rpc.RunNock qualified as Rpc
import Data.ByteString.Base64 qualified as Base64
import Juvix.Compiler.Nockma.Encoding.Cue (DecodingError, cueFromByteString'')
import Juvix.Compiler.Nockma.Encoding.Jam (jamToByteString)
import Juvix.Compiler.Nockma.Language (NockNaturalNaturalError)
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Data.CodeAnn (simpleErrorCodeAnn)
import Juvix.Prelude
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

fromJSON :: (Members '[Error SimpleError] r) => (Aeson.FromJSON a) => Value -> Sem r a
fromJSON v = case Aeson.fromJSON v of
  Aeson.Success r -> return r
  Aeson.Error err -> throw (SimpleError (mkAnsiText err))

runNockma ::
  (Members '[Anoma, Error SimpleError] r) =>
  Nockma.Term Natural ->
  [Nockma.Term Natural] ->
  Sem r (Nockma.Term Natural)
runNockma prog inputs = do
  let prog' = encodeJam64 prog
      args = map (Rpc.NockInputJammed . encodeJam64) inputs
      msg =
        Rpc.RunNock
          { _runNockJammedProgram = prog',
            _runNockPrivateInputs = args,
            _runNockPublicInputs = []
          }
  let json = Aeson.toJSON msg
  res :: Rpc.Response <- anomaRpc json >>= fromJSON
  decodeJam64 (res ^. Rpc.proof)
