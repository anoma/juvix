-- | This module assumes that the following external dependencies are installed:
-- 1. mix
--
-- 2. grpcurl
module Anoma.Effect.Base
  ( Anoma,
    anomaRpc,
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
import Juvix.Compiler.Nockma.Translation.FromTree (AnomaResult)
import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value, eitherDecodeStrict, encode)

data Anoma :: Effect where
  -- | grpc call
  AnomaRpc :: GrpcMethodUrl -> Value -> Anoma m Value

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

runAnomaEphemeral :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnomaEphemeral anomapath body = runReader anomapath . runProcess $ do
  cproc <- anomaClientCreateProcess LaunchModeAttached
  withCreateProcess cproc $ \_stdin mstdout _stderr _procHandle -> do
    grpcServer <- setupAnomaClientProcess (fromJust mstdout)
    runReader grpcServer $ do
      (`interpret` inject body) $ \case
        AnomaRpc method i -> anomaRpc' method i

runAnomaWithClient :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaClientInfo -> Sem (Anoma ': r) a -> Sem r a
runAnomaWithClient grpcInfo body =
  runProcess
    . runReader grpcInfo
    $ (`interpret` inject body)
    $ \case
      AnomaRpc method i -> anomaRpc' method i
