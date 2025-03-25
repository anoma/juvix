-- | This module assumes that the following external dependencies are installed:
-- 1. mix
--
-- 2. grpcurl
module Anoma.Effect.Base
  ( Anoma,
    anomaRpc,
    anomaListMethods,
    getNodeInfo,
    runAnomaEphemeral,
    runAnomaWithClient,
    fromJSONErr,
    logMessageValue,
    module Anoma.Rpc.Base,
    module Anoma.Client.Base,
  )
where

import Anoma.Client.Base
import Anoma.Rpc.Base
import Data.ByteString qualified as B
import Data.Text qualified as T
import Effectful.Environment
import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value, eitherDecodeStrict, encode)
import Juvix.Prelude.Aeson qualified as Aeson

data Anoma :: Effect where
  -- | gRPC call
  AnomaRpc :: GrpcMethodUrl -> Value -> Anoma m Value
  -- | List all gRPC methods using server reflection
  AnomaListMethods :: Anoma m [GrpcMethodUrl]
  -- | Get information on the node that the client sends messages to
  GetNodeInfo :: Anoma m NodeInfo

makeSem ''Anoma

anomaRpc' ::
  (Members '[Reader AnomaClientInfo, Process, EmbedIO, Error SimpleError] r) =>
  GrpcMethodUrl ->
  Value ->
  Sem r Value
anomaRpc' method payload = do
  cproc <- grpcCliProcess method
  withCreateProcess cproc $ \mstdin mstdout _stderr _procHandle -> do
    let stdinH = fromJust mstdin
        stdoutH = fromJust mstdout
        inputbs = B.toStrict (encode payload)
    liftIO (B.hPutStr stdinH inputbs)
    hClose stdinH
    res <- eitherDecodeStrict <$> liftIO (B.hGetContents stdoutH)
    case res of
      Right r -> return r
      Left err -> throw (SimpleError (mkAnsiText err))

anomaListMethods' ::
  (Members '[Reader AnomaClientInfo, Process, Error SimpleError] r) =>
  Sem r [GrpcMethodUrl]
anomaListMethods' = do
  cproc <- grpcCliListProcess
  (exitCode, stdOut, stdErr) <- readCreateProcessWithExitCode cproc ""
  case exitCode of
    ExitFailure {} -> throw (SimpleError (mkAnsiText ("gRPC list failed: " <> stdErr)))
    ExitSuccess -> return (mapMaybe parseMethod (lines stdOut))
      where
        parseMethod :: String -> Maybe GrpcMethodUrl
        parseMethod = fmap mkGrpcMethodUrl . nonEmpty . T.split (== '.') . pack

grpcCliProcess :: (Members '[Reader AnomaClientInfo] r) => GrpcMethodUrl -> Sem r CreateProcess
grpcCliProcess method = do
  grpcPort <- asks (^. anomaClientInfoPort)
  grpcUrl <- asks (^. anomaClientInfoUrl)
  return
    ( proc
        "grpcurl"
        [ "-d",
          "@",
          "-plaintext",
          grpcUrl <> ":" <> show grpcPort,
          show method
        ]
    )
      { std_in = CreatePipe,
        std_out = CreatePipe
      }

grpcCliListProcess :: (Members '[Reader AnomaClientInfo] r) => Sem r CreateProcess
grpcCliListProcess = do
  grpcPort <- asks (^. anomaClientInfoPort)
  grpcUrl <- asks (^. anomaClientInfoUrl)
  return
    ( proc
        "grpcurl"
        [ "-plaintext",
          grpcUrl <> ":" <> show grpcPort,
          "list"
        ]
    )

runAnomaEphemeral :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnomaEphemeral anomapath body = runEnvironment . runReader anomapath . runProcess $ do
  cproc <- anomaClientCreateProcess LaunchModeAttached
  withCreateProcess cproc $ \_stdin mstdout _stderr _procHandle -> do
    grpcInfo <- hardcodeNodeId <$> setupAnomaClientProcess (fromJust mstdout)
    runReader grpcInfo $ do
      (`interpret` inject body) $ \case
        AnomaRpc method i -> anomaRpc' method i
        AnomaListMethods -> anomaListMethods'
        GetNodeInfo -> return NodeInfo {_nodeInfoId = grpcInfo ^. anomaClientInfoNodeId}

runAnomaWithClient :: forall r a. (Members '[EmbedIO, Error SimpleError] r) => AnomaClientInfo -> Sem (Anoma ': r) a -> Sem r a
runAnomaWithClient grpcInfo body = do
  let grpcInfo' = hardcodeNodeId grpcInfo
  runProcess
    . runReader grpcInfo'
    $ (`interpret` inject body)
    $ \case
      AnomaRpc method i -> anomaRpc' method i
      AnomaListMethods -> anomaListMethods'
      GetNodeInfo -> return NodeInfo {_nodeInfoId = grpcInfo' ^. anomaClientInfoNodeId}

fromJSONErr :: (Members '[Error SimpleError] r) => (Aeson.FromJSON a) => Value -> Sem r a
fromJSONErr v = case Aeson.fromJSON v of
  Aeson.Success r -> return r
  Aeson.Error err -> throw (SimpleError (mkAnsiText err))

logMessageValue :: (Aeson.ToJSON val, Member Logger r) => Text -> val -> Sem r ()
logMessageValue title val = logVerbose (mkAnsiText (annotate AnnImportant (pretty title <> ":\n") <> pretty (Aeson.jsonEncodeToPrettyText val)))

-- 2024-11-27: This node id is hardcoded for now because:
-- 1. The gRPC client crashes if the Anoma client node_id is not a valid base64 string
-- 2. The client node_id is frequently not a valid base64 string
-- 2. The node_id is unused in gRPC requests so will likely be removed
--    See: https://github.com/anoma/anoma/issues/1635
hardcodeNodeId :: AnomaClientInfo -> AnomaClientInfo
hardcodeNodeId = set anomaClientInfoNodeId "40872587"
