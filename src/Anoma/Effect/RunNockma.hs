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
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
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
  logVerbose ("Request: " <> show runNockUrl)
  logMessageValue "Request Payload" msg
  resVal :: Value <- anomaPost runNockUrl (Aeson.toJSON msg) >>= fromJSONErr
  logMessageValue "Response Payload" resVal
  res :: Response <- case Aeson.fromJSON resVal of
    Aeson.Success r
      | r ^. successResult /= "error" -> return $ ResponseSuccess r
    _ -> do
      r <- fromJSONErr resVal
      return $ ResponseError r
  case res of
    ResponseSuccess s -> do
      result <- decodeCue64 (s ^. successResult)
      traces <- mapM decodeCue64 (s ^. successTraces)
      return
        RunNockmaResult
          { _runNockmaResult = result,
            _runNockmaTraces = traces
          }
    ResponseError err -> do
      traces <- mapM decodeCue64 (err ^. errorTraces)
      let atomTraces = mapMaybe ppAtomAsText traces
      throw
        . SimpleError
        . mkAnsiText @(Doc Ann)
        $ annotate AnnError "runNockma failed"
          <> annotate AnnKeyword "\nError:\n"
          <> pretty (err ^. errorError)
          <> annotate
            AnnKeyword
            ( "\n\nTraces ("
                <> show (length traces)
                <> "):\n"
            )
          <> vsepHard (map (pretty . Nockma.ppTrace) traces)
          <> annotate
            AnnKeyword
            ( "\n\nAtom traces as Text ("
                <> show (length atomTraces)
                <> "):\n"
            )
          <> vsepHard (mapMaybe ppAtomAsText traces)
  where
    ppAtomAsText :: Nockma.Term Natural -> Maybe (Doc CodeAnn)
    ppAtomAsText = \case
      Nockma.TermCell {} -> Nothing
      Nockma.TermAtom a ->
        fmap (annotate AnnLiteralString . pretty) . run . runFail . failFromError @Nockma.NockNaturalNaturalError $
          atomToText a

    prepareArgument :: RunNockmaArg -> Text
    prepareArgument =
      \case
        RunNockmaArgTerm t -> encodeJam64 t
        RunNockmaArgJammed a -> decodeUtf8 (Base64.encode a)
