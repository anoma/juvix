-- | This module assumes that the following external dependencies are installed:
-- 1. mix
--
-- 2. grpcurl
module Anoma.Effect.Base
  ( Anoma,
    getAnomaProcesses,
    anomaRpc,
    AnomaPath (..),
    AnomaProcesses (..),
    anomaNodeHandle,
    anomaClientHandle,
    anomaPath,
    runAnoma,
    runAnomaTest,
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

data AnomaProcesses = AnomaProcesses
  { _anomaNodeHandle :: ProcessHandle,
    _anomaClientHandle :: ProcessHandle
  }

data Anoma :: Effect where
  -- | Keep the node and client running
  GetAnomaProcesses :: Anoma m AnomaProcesses
  -- | grpc call
  AnomaRpc :: GrpcMethodUrl -> Value -> Anoma m Value

makeSem ''Anoma
makeLenses ''AnomaProcesses

newtype AnomaPath = AnomaPath {_anomaPath :: Path Abs Dir}

newtype GrpcPort = GrpcPort {_grpcPort :: Int}

makeLenses ''AnomaPath
makeLenses ''GrpcPort

listenPort :: Int
listenPort = 50051

relativeToAnomaDir :: (Members '[Reader AnomaPath] r) => Path Rel x -> Sem r (Path Abs x)
relativeToAnomaDir p = do
  anoma <- asks (^. anomaPath)
  return (anoma <//> p)

withSpawnAnomaClient ::
  (Members '[Process, Logger, EmbedIO, Reader AnomaPath, Reader GrpcPort, Error SimpleError] r) =>
  (ProcessHandle -> Sem r a) ->
  Sem r a
withSpawnAnomaClient body = do
  cprocess <- mkProcess
  withCreateProcess cprocess $ \_stdin mstdout _stderr procHandle -> do
    let out = fromJust mstdout
    txt <- hGetLine out
    case takeWhile (/= '.') (unpack txt) of
      "Connected to node" -> do
        logInfo "Anoma client successfully started"
        logInfo (mkAnsiText ("Listening on port " <> annotate AnnImportant (pretty listenPort)))
        body procHandle
      _ -> throw (SimpleError (mkAnsiText @Text "Something went wrong when starting the anoma client"))
  where
    mkProcess :: (Members '[Reader AnomaPath, Reader GrpcPort] r') => Sem r' CreateProcess
    mkProcess = do
      grpcport <- asks (^. grpcPort)
      anomaClient <- relativeToAnomaDir clientRelFile
      return
        ( proc
            (toFilePath anomaClient)
            [ "--listen-port",
              show listenPort,
              "--node-host",
              "localhost",
              "--node-port",
              show grpcport
            ]
        )
          { std_out = CreatePipe
          }

withSpawnAnomaNode ::
  forall r a.
  (Members '[EmbedIO, Logger, Error SimpleError, Process, Reader AnomaPath] r) =>
  (Int -> Handle -> ProcessHandle -> Sem r a) ->
  Sem r a
withSpawnAnomaNode body = withSystemTempFile "start.exs" $ \fp tmpHandle -> do
  liftIO (B.hPutStr tmpHandle anomaStartExs)
  hClose tmpHandle
  cproc <- cprocess (toFilePath fp)
  withCreateProcess cproc $ \_stdin mstdout _stderr procHandle -> do
    let nodeOut = fromJust mstdout
    ln <- hGetLine nodeOut
    let parseError = throw (SimpleError (mkAnsiText ("Failed to parse the grpc port when starting the anoma client.\nExpected a number but got " <> ln)))
    nodeport :: Int <- either (const parseError) return . readEither . unpack $ ln
    logInfo "Anoma node successfully started"
    body nodeport (fromJust mstdout) procHandle
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
  (Members '[Reader AnomaPath, Process, EmbedIO, Error SimpleError] r) =>
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

grpcCliProcess :: (Members '[Reader AnomaPath] r) => GrpcMethodUrl -> Sem r CreateProcess
grpcCliProcess method = do
  importPath <- relativeToAnomaDir relProtoDir
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
          "localhost:" <> show listenPort,
          show method
        ]
    )
      { std_in = CreatePipe,
        std_out = CreatePipe
      }

-- | Assumes the node and client are already running
runAnomaTest :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnomaTest anomapath body = runReader anomapath . runProcess $
  (`interpret` inject body) $ \case
    GetAnomaProcesses -> error "unsupported"
    AnomaRpc method i -> anomaRpc' method i

runAnoma :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnoma anomapath body = runReader anomapath . runProcess $
  withSpawnAnomaNode $ \grpcport _nodeOut nodeH ->
    runReader (GrpcPort grpcport) $
      withSpawnAnomaClient $ \clientH -> do
        (`interpret` inject body) $ \case
          GetAnomaProcesses ->
            return
              AnomaProcesses
                { _anomaNodeHandle = nodeH,
                  _anomaClientHandle = clientH
                }
          AnomaRpc method i -> anomaRpc' method i
