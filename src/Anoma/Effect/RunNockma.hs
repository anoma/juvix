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
import Juvix.Prelude.Aeson (ToJSON, Value)
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
  forall r.
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
  let logValue :: (ToJSON val) => Text -> val -> Sem r ()
      logValue title val = logVerbose (mkAnsiText (annotate AnnImportant (pretty title <> ":\n") <> pretty (Aeson.jsonEncodeToPrettyText val)))
  logValue "Request Payload" msg
  resVal :: Value <- anomaRpc runNockGrpcUrl (Aeson.toJSON msg) >>= fromJSON
  logValue "Request Payload" resVal
  res :: Response <- fromJSON resVal
  case res of
    ResponseProof x -> decodeCue64 x
    ResponseError err -> throw (SimpleError (mkAnsiText err))
