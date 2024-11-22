-- | This module assumes that the following external dependencies are installed:
-- 1. mix
--
-- 2. grpcurl
module Anoma.Effect.Base
  ( Anoma,
    anomaRpc,
    anomaListMethods,
    runAnomaEphemeral,
    runAnomaWithClient,
    module Anoma.Rpc.Base,
    module Anoma.Client.Base,
    module Juvix.Compiler.Nockma.Translation.FromTree,
  )
where

import Anoma.Client.Base
import Anoma.Rpc.Base
import Data.ByteString qualified as B
import Data.Text qualified as T
import Juvix.Compiler.Nockma.Translation.FromTree (AnomaResult)
import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value, eitherDecodeStrict, encode)

data Anoma :: Effect where
  -- | gRPC call
  AnomaRpc :: GrpcMethodUrl -> Value -> Anoma m Value
  -- | List all gRPC methods using server reflection
  AnomaListMethods :: Anoma m [GrpcMethodUrl]

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
runAnomaEphemeral anomapath body = runReader anomapath . runProcess $ do
  cproc <- anomaClientCreateProcess LaunchModeAttached
  withCreateProcess cproc $ \_stdin mstdout _stderr _procHandle -> do
    grpcServer <- setupAnomaClientProcess (fromJust mstdout)
    runReader grpcServer $ do
      (`interpret` inject body) $ \case
        AnomaRpc method i -> anomaRpc' method i
        AnomaListMethods -> anomaListMethods'

runAnomaWithClient :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaClientInfo -> Sem (Anoma ': r) a -> Sem r a
runAnomaWithClient grpcInfo body =
  runProcess
    . runReader grpcInfo
    $ (`interpret` inject body)
    $ \case
      AnomaRpc method i -> anomaRpc' method i
      AnomaListMethods -> anomaListMethods'
