module Anoma.Effect.Base
  ( Anoma,
    noHalt,
    anomaRpc,
    AnomaPath (..),
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

data Anoma :: Effect where
  -- | Keep the node and client running
  NoHalt :: Anoma m ExitCode
  -- | Blocking rpc call
  AnomaRpc :: GrpcMethodUrl -> Value -> Anoma m Value

makeSem ''Anoma

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

withSapwnAnomaNode ::
  (Members '[EmbedIO, Logger, Process, Reader AnomaPath] r) =>
  (Int -> Handle -> ProcessHandle -> Sem r a) ->
  Sem r a
withSapwnAnomaNode body = withSystemTempFile "start.exs" $ \fp tmpHandle -> do
  liftIO (B.hPutStr tmpHandle anomaStartExs)
  hClose tmpHandle
  curDir <- getCurrentDir
  asks (^. anomaPath) >>= setCurrentDir
  withCreateProcess (cprocess (toFilePath fp)) $ \_stdin mstdout _stderr procHandle -> do
    setCurrentDir curDir
    let nodeOut = fromJust mstdout
    grpcNode :: Int <- either (error . pack) id . readEither . unpack <$> hGetLine nodeOut
    logInfo "Anoma node successfully started"
    body grpcNode (fromJust mstdout) procHandle
  where
    cprocess :: FilePath -> CreateProcess
    cprocess exs =
      (proc "mix" ["run", "--no-halt", exs])
        { std_out = CreatePipe
        }

anomaRpc' :: (Members '[Reader AnomaPath, Process, EmbedIO, Error SimpleError] r) => GrpcMethodUrl -> Value -> Sem r Value
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
  paths <- relativeToAnomaDir relProtoDir
  return
    ( proc
        "grpc_cli"
        [ "call",
          "--json_input",
          "--json_output",
          "--proto_path",
          toFilePath paths,
          "--protofiles",
          toFilePath relProtoFile,
          "localhost:" <> show listenPort,
          show method
        ]
    )
      { std_in = CreatePipe,
        std_out = CreatePipe
      }

runAnoma :: forall r a. (Members '[Logger, EmbedIO, Error SimpleError] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnoma anomapath body = runReader anomapath . runConcurrent . runProcess $
  withSapwnAnomaNode $ \grpcport _nodeOut nodeH ->
    runReader (GrpcPort grpcport) $
      withSpawnAnomaClient $ \_clientH -> do
        (`interpret` inject body) $ \case
          NoHalt -> waitForProcess nodeH
          AnomaRpc method i -> anomaRpc' method i
