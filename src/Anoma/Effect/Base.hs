-- | This module assumes that the following external dependencies are installed:
-- 1. mix
--
-- 2. grpcurl
module Anoma.Effect.Base
  ( Anoma,
    getAnomaProcess,
    anomaRpc,
    AnomaPath (..),
    AnomaProcess (..),
    anomaProcessHandle,
    anomaPath,
    runAnoma,
    module Anoma.Rpc.Base,
    module Juvix.Compiler.Nockma.Translation.FromTree,
  )
where

import Anoma.Effect.Paths
import Anoma.Rpc.Base
import Data.ByteString qualified as B
import Juvix.Compiler.Nockma.Translation.FromTree (AnomaResult)
import Juvix.Data.CodeAnn
import Juvix.Extra.Paths (anomaStartExs)
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value, eitherDecodeStrict, encode)

newtype AnomaProcess = AnomaProcess
  { _anomaProcessHandle :: ProcessHandle
  }

data Anoma :: Effect where
  -- | Keep the node and client running
  GetAnomaProcess :: Anoma m AnomaProcess
  -- | grpc call
  AnomaRpc :: GrpcMethodUrl -> Value -> Anoma m Value

makeSem ''Anoma
makeLenses ''AnomaProcess

newtype AnomaPath = AnomaPath {_anomaPath :: Path Abs Dir}

newtype ClientGrpcPort = ClientGrpcPort {_clientGrpcPort :: Int}

makeLenses ''AnomaPath
makeLenses ''ClientGrpcPort

relativeToAnomaDir :: (Members '[Reader AnomaPath] r) => Path Rel x -> Sem r (Path Abs x)
relativeToAnomaDir p = do
  anoma <- asks (^. anomaPath)
  return (anoma <//> p)

withSpawnAnomaNodeAndClient ::
  forall r a.
  (Members '[EmbedIO, Logger, Error SimpleError, Process, Reader AnomaPath] r) =>
  (ClientGrpcPort -> ProcessHandle -> Sem r a) ->
  Sem r a
withSpawnAnomaNodeAndClient body = withSystemTempFile "start.exs" $ \fp tmpHandle -> do
  liftIO (B.hPutStr tmpHandle anomaStartExs) >> hClose tmpHandle
  cproc <- cprocess (toFilePath fp)
  withCreateProcess cproc $ \_stdin mstdout _stderr procHandle -> do
    let nodeOut = fromJust mstdout
    ln <- hGetLine nodeOut
    let parseError = throw (SimpleError (mkAnsiText ("Failed to parse the client grpc port when starting the anoma node and client.\nExpected a number but got " <> ln)))
    grpcPort :: Int <- either (const parseError) return . readEither . unpack $ ln
    logInfo "Anoma node and client successfully started"
    logInfo (mkAnsiText ("Listening on port " <> annotate AnnImportant (pretty grpcPort)))
    body (ClientGrpcPort grpcPort) procHandle
  where
    cprocess :: (Members '[Reader AnomaPath] r') => FilePath -> Sem r' CreateProcess
    cprocess exs = do
      anomapath <- asks (^. anomaPath)
      return
        (proc "mix" ["run", "--no-halt", exs])
          { std_out = CreatePipe,
            cwd = Just (toFilePath anomapath)
          }

anomaRpc' ::
  (Members '[Reader ClientGrpcPort, Reader AnomaPath, Process, EmbedIO, Error SimpleError] r) =>
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

grpcCliProcess :: (Members '[Reader ClientGrpcPort, Reader AnomaPath] r) => GrpcMethodUrl -> Sem r CreateProcess
grpcCliProcess method = do
  importPath <- relativeToAnomaDir relProtoDir
  grpcPort <- asks (^. clientGrpcPort)
  return
    ( proc
        "grpcurl"
        [ "-import-path",
          toFilePath importPath,
          "-proto",
          toFilePath relProtoFile,
          "-d",
          "@",
          "-plaintext",
          "localhost:" <> show grpcPort,
          show method
        ]
    )
      { std_in = CreatePipe,
        std_out = CreatePipe
      }

runAnoma :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnoma anomapath body = runReader anomapath . runProcess $
  withSpawnAnomaNodeAndClient $ \grpcport anomaH ->
    runReader grpcport $ do
      (`interpret` inject body) $ \case
        GetAnomaProcess ->
          return
            AnomaProcess
              { _anomaProcessHandle = anomaH
              }
        AnomaRpc method i -> anomaRpc' method i
