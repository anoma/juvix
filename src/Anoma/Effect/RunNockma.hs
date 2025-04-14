module Anoma.Effect.RunNockma
  ( module Anoma.Effect.RunNockma,
    module Anoma.Http.RunNock,
  )
where

import Anoma.Effect.Base
import Anoma.Http.RunNock
import Data.ByteString.Base64 qualified as Base64
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value)
import Juvix.Prelude.Aeson qualified as Aeson

-- | An argument to a program sent to the RunNock endpoint
data RunNockmaArg
  = -- | An argument that must be jammed before it is sent
    RunNockmaArgTerm (Nockma.Term Natural)
  | -- | An argument that is already jammed and must not be jammed again before it is sent
    RunNockmaArgJammed ByteString

data RunNockmaInput = RunNockmaInput
  { _runNockmaProgram :: Nockma.Term Natural,
    _runNockmaArgs :: [RunNockmaArg]
  }

data RunNockmaResult = RunNockmaResult
  { _runNockmaResult :: Nockma.Term Natural,
    _runNockmaTraces :: [Nockma.Term Natural]
  }

makeLenses ''RunNockmaInput
makeLenses ''RunNockmaResult

runNockma ::
  forall r.
  (Members '[Anoma, Error SimpleError, Logger] r) =>
  RunNockmaInput ->
  Sem r RunNockmaResult
runNockma i = do
  let prog' = encodeJam64 (i ^. runNockmaProgram)
      args = map prepareArgument (i ^. runNockmaArgs)
      msg =
        RunNock
          { _runNockJammedProgram = prog',
            _runNockPrivateInputs = args,
            _runNockPublicInputs = []
          }
  logMessageValue "Request Payload" msg
  resVal :: Value <- anomaPost runNockUrl (Aeson.toJSON msg) >>= fromJSONErr
  logMessageValue "Response Payload" resVal
  res :: Response <- fromJSONErr resVal
  case res of
    ResponseSuccess s -> do
      result <- decodeCue64 (s ^. successResult)
      traces <- mapM decodeCue64 (s ^. successTraces)
      return
        RunNockmaResult
          { _runNockmaResult = result,
            _runNockmaTraces = traces
          }
    ResponseError err -> throw (SimpleError (mkAnsiText @Text "runNockma failed:\n" <> mkAnsiText (err ^. errorError)))
  where
    prepareArgument :: RunNockmaArg -> Text
    prepareArgument =
      \case
        RunNockmaArgTerm t -> encodeJam64 t
        RunNockmaArgJammed a -> decodeUtf8 (Base64.encode a)
