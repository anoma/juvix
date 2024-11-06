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
  { _runNockmaProgram :: Nockma.Term Natural,
    _runNockmaArgs :: [Nockma.Term Natural]
  }

data RunNockmaResult = RunNockmaResult
  { _runNockmaResult :: Nockma.Term Natural,
    _runNockmaTraces :: [Nockma.Term Natural]
  }

makeLenses ''RunNockmaInput
makeLenses ''RunNockmaResult

fromJSON :: (Members '[Error SimpleError, Logger] r) => (Aeson.FromJSON a) => Value -> Sem r a
fromJSON v = case Aeson.fromJSON v of
  Aeson.Success r -> return r
  Aeson.Error err -> throw (SimpleError (mkAnsiText err))

runNockma ::
  forall r.
  (Members '[Anoma, Error SimpleError, Logger] r) =>
  RunNockmaInput ->
  Sem r RunNockmaResult
runNockma i = do
  let prog' = encodeJam64 (i ^. runNockmaProgram)
      args = map (NockInputJammed . encodeJam64) (i ^. runNockmaArgs)
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
  logValue "Response Payload" resVal
  res :: Response <- fromJSON resVal
  case res of
    ResponseSuccess s -> do
      result <- decodeCue64 (s ^. successResult)
      traces <- mapM decodeCue64 (s ^. successTraces)
      return
        RunNockmaResult
          { _runNockmaResult = result,
            _runNockmaTraces = traces
          }
    ResponseError err -> throw (SimpleError (mkAnsiText err))
