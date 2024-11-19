-- | This module assumes that the following external dependencies are installed:
-- 1. mix
--
-- 2. grpcurl
module Anoma.Effect.Base
  ( Anoma,
    anomaRpc,
    AnomaPath (..),
    AnomaProcess (..),
    AnomaGrpcClientInfo (..),
    anomaGrpcClientInfoPort,
    anomaGrpcClientInfoUrl,
    anomaProcessHandle,
    anomaPath,
    runAnomaEphemeral,
    runAnomaPersistent,
    module Anoma.Rpc.Base,
    module Juvix.Compiler.Nockma.Translation.FromTree,
  )
where

import Anoma.Rpc.Base
import Data.ByteString qualified as B
import Data.Text qualified as T
import Juvix.Compiler.Nockma.Translation.FromTree (AnomaResult)
import Juvix.Data.CodeAnn
import Juvix.Extra.Paths (anomaStartExs)
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value, eitherDecodeStrict, encode)

newtype AnomaProcess = AnomaProcess
  { _anomaProcessHandle :: ProcessHandle
  }

data AnomaGrpcClientInfo = AnomaGrpcClientInfo
  { _anomaGrpcClientInfoPort :: Int,
    _anomaGrpcClientInfoUrl :: String
  }

data Anoma :: Effect where
  -- | grpc call
  AnomaRpc :: GrpcMethodUrl -> Value -> Anoma m Value

makeSem ''Anoma
makeLenses ''AnomaProcess

newtype AnomaPath = AnomaPath {_anomaPath :: Path Abs Dir}

makeLenses ''AnomaPath
makeLenses ''AnomaGrpcClientInfo

anomaCreateProcess :: (Members '[Reader AnomaPath] r') => Sem r' CreateProcess
anomaCreateProcess = do
  anomapath <- asks (^. anomaPath)
  return
    (proc "mix" ["run", "--no-halt", "-e", unpack (T.strip (decodeUtf8 anomaStartExs))])
      { std_out = CreatePipe,
        cwd = Just (toFilePath anomapath)
      }

setupAnomaProcess :: (Members '[EmbedIO, Logger, Error SimpleError] r) => Handle -> Sem r AnomaGrpcClientInfo
setupAnomaProcess nodeOut = do
  ln <- hGetLine nodeOut
  let parseError = throw (SimpleError (mkAnsiText ("Failed to parse the client grpc port when starting the anoma node and client.\nExpected a number but got " <> ln)))
  grpcPort :: Int <- either (const parseError) return . readEither . unpack $ ln
  logInfo "Anoma node and client successfully started"
  logInfo (mkAnsiText ("Listening on port " <> annotate AnnImportant (pretty grpcPort)))
  return
    ( AnomaGrpcClientInfo
        { _anomaGrpcClientInfoPort = grpcPort,
          _anomaGrpcClientInfoUrl = "localhost"
        }
    )

anomaRpc' ::
  (Members '[Reader AnomaGrpcClientInfo, Process, EmbedIO, Error SimpleError] r) =>
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

grpcCliProcess :: (Members '[Reader AnomaGrpcClientInfo] r) => GrpcMethodUrl -> Sem r CreateProcess
grpcCliProcess method = do
  grpcPort <- asks (^. anomaGrpcClientInfoPort)
  grpcUrl <- asks (^. anomaGrpcClientInfoUrl)
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
  cproc <- anomaCreateProcess
  withCreateProcess cproc $ \_stdin mstdout _stderr _procHandle -> do
    grpcServer <- setupAnomaProcess (fromJust mstdout)
    runReader grpcServer $ do
      (`interpret` inject body) $ \case
        AnomaRpc method i -> anomaRpc' method i

runAnomaPersistent :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnomaPersistent anomapath body = runConcurrent . runReader anomapath . runProcess $
   bracket setup teardown handler
      where
        setup :: (Members '[EmbedIO, Logger, Reader AnomaPath, Process, Concurrent, Error SimpleError] x) => Sem x (ProcessHandle, AnomaGrpcClientInfo)
        setup = do
          cproc <- anomaCreateProcess
          (_stdin, mstdout, _stderr, procHandle) <- createProcess cproc
          grpcServer <- setupAnomaProcess (fromJust mstdout)
          return (procHandle, grpcServer)

        teardown :: (Members '[Concurrent, Process] x) => (ProcessHandle, AnomaGrpcClientInfo) -> Sem x ()
        teardown (process, _) = do
          terminateProcess process

        handler :: forall r'. (Subset r (Anoma ': r'), Members '[Error SimpleError, EmbedIO, Process] r')
             => (ProcessHandle, AnomaGrpcClientInfo) -> Sem r' a
        handler (process, grpcServer) = runReader grpcServer $ do
          res <- (`interpret` body') $ \case
            AnomaRpc method i -> anomaRpc' method i
          void (waitForProcess process)
          return res
          where
            body' :: Sem (Anoma ': Reader AnomaGrpcClientInfo ': r') a
            body' = (raiseUnder . inject) body
