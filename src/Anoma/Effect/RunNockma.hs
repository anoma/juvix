module Anoma.Effect.RunNockma
  ( module Anoma.Effect.RunNockma,
    module Anoma.Rpc.RunNock,
  )
where

import Anoma.Effect.Base
import Anoma.Rpc.RunNock
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value)
import Juvix.Prelude.Aeson qualified as Aeson

data RunNockmaInput = RunNockmaInput
  { _runNockmaProgram :: AnomaResult,
    _runNockmaInput :: [Nockma.Term Natural]
  }

makeLenses ''RunNockmaInput

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
