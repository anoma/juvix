-- | This module assumes that the following external dependencies are installed:
-- 1. mix
-- 2. curl
module Anoma.Effect.Base
  ( Anoma,
    anomaPost,
    anomaGet,
    anomaCheck,
    runAnomaEphemeral,
    runAnomaWithClient,
    fromJSONErr,
    logMessageValue,
    withLoggerThread,
    module Anoma.Http.Base,
    module Anoma.Client.Base,
  )
where

import Anoma.Client.Base
import Anoma.Http.Base
import Data.ByteString qualified as B
import Effectful.Environment
import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value, eitherDecodeStrict, encode)
import Juvix.Prelude.Aeson qualified as Aeson

data Anoma :: Effect where
  -- | HTTP POST call
  AnomaPost :: EndpointUrl -> Value -> Anoma m Value
  -- | HTTP GET call
  AnomaGet :: EndpointUrl -> Anoma m Value
  -- | Check if the client is running
  AnomaCheck :: Anoma m Bool

makeSem ''Anoma

anomaRequest' ::
  (Members '[Reader AnomaClientInfo, Process, EmbedIO, Error SimpleError] r) =>
  HttpMethod ->
  EndpointUrl ->
  Maybe Value ->
  Sem r Value
anomaRequest' method endpoint mpayload = do
  cproc <- httpCliProcess method endpoint
  withCreateProcess cproc $ \mstdin mstdout _stderr _procHandle -> do
    whenJust mpayload $ \payload -> do
      let stdinH = fromJust mstdin
          inputbs = B.toStrict (encode payload)
      liftIO (B.hPutStr stdinH inputbs)
      hClose stdinH
    let stdoutH = fromJust mstdout
    res <- eitherDecodeStrict <$> liftIO (B.hGetContents stdoutH)
    case res of
      Right r -> return r
      Left err -> throw (SimpleError (mkAnsiText err))

anomaCheck' ::
  (Members '[Reader AnomaClientInfo, Process, Error SimpleError] r) =>
  Sem r Bool
anomaCheck' = do
  cproc <- httpCliCheckProcess
  (exitCode, _, stdErr) <- readCreateProcessWithExitCode cproc ""
  case exitCode of
    ExitFailure {} -> throw (SimpleError (mkAnsiText ("HTTP request failed: " <> stdErr)))
    ExitSuccess -> return True

httpCliProcess :: (Members '[Reader AnomaClientInfo] r) => HttpMethod -> EndpointUrl -> Sem r CreateProcess
httpCliProcess method endpoint = do
  httpPort <- asks (^. anomaClientInfoPort)
  httpUrl <- asks (^. anomaClientInfoUrl)
  let url = httpUrl <> ":" <> show httpPort <> "/" <> show endpoint
      args = case method of
        HttpGet -> ["-s", "-X", "GET", url, "-H", "accept: application/json"]
        HttpPost -> ["-s", "-X", "POST", url, "-H", "accept: application/json", "-H", "Content-Type: application/json", "-d", "@-"]
  return
    (proc "curl" args)
      { std_in =
          case method of
            HttpGet -> NoStream
            HttpPost -> CreatePipe,
        std_out = CreatePipe
      }

httpCliCheckProcess :: (Members '[Reader AnomaClientInfo] r) => Sem r CreateProcess
httpCliCheckProcess = do
  httpPort <- asks (^. anomaClientInfoPort)
  httpUrl <- asks (^. anomaClientInfoUrl)
  return
    ( proc
        "curl"
        [ "-s",
          "-I",
          httpUrl <> ":" <> show httpPort
        ]
    )

-- | TODO Consider using Logger effect instead of putStrLn
withLoggerThread :: forall r a. (Members '[Concurrent, EmbedIO] r) => Handle -> Sem r a -> Sem r a
withLoggerThread h m = withAsync catLines (const m)
  where
    catLines :: Sem r ()
    catLines = forever $ do
      t <- hGetLine h
      putStrLn t

runAnomaEphemeral :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnomaEphemeral anomapath body = runEnvironment . runReader anomapath . runProcess $ do
  cproc <- anomaClientCreateProcess LaunchModeAttached
  withCreateProcess cproc $ \_stdin mstdout mstderr _procHandle -> do
    let stdOut = fromJust mstdout
        stdErr = fromJust mstderr
    anomaInfo <- setupAnomaClientProcess stdOut
    runConcurrent . withLoggerThread stdOut . withLoggerThread stdErr $
      runReader anomaInfo $ do
        (`interpret` inject body) $ \case
          AnomaPost url i -> anomaRequest' HttpPost url (Just i)
          AnomaGet url -> anomaRequest' HttpGet url Nothing
          AnomaCheck -> anomaCheck'

runAnomaWithClient :: forall r a. (Members '[EmbedIO, Error SimpleError] r) => AnomaClientInfo -> Sem (Anoma ': r) a -> Sem r a
runAnomaWithClient anomaInfo body = do
  runProcess
    . runReader anomaInfo
    $ (`interpret` inject body)
    $ \case
      AnomaPost url i -> anomaRequest' HttpPost url (Just i)
      AnomaGet url -> anomaRequest' HttpGet url Nothing
      AnomaCheck -> anomaCheck'

fromJSONErr :: (HasCallStack, Members '[Error SimpleError] r) => (Aeson.FromJSON a) => Value -> Sem r a
fromJSONErr v = case Aeson.fromJSON v of
  Aeson.Success r -> return r
  Aeson.Error err ->
    throw . SimpleError $
      mkAnsiText ghcCallStack
        <> mkAnsiText ("\nTried to parse: " <> Aeson.jsonEncodeToText v)
        <> mkAnsiText ("\nError message: " <> err)

logMessageValue :: (Aeson.ToJSON val, Member Logger r) => Text -> val -> Sem r ()
logMessageValue title val = logVerbose (mkAnsiText (annotate AnnImportant (pretty title <> ":\n") <> pretty (Aeson.jsonEncodeToPrettyText val)))
