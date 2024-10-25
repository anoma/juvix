module Anoma.Effect.Base
  ( Anoma,
    noHalt,
    anomaRpc,
    AnomaPath (..),
    anomaPath,
    runAnoma,
    module Juvix.Prelude.Aeson,
    module Juvix.Compiler.Nockma.Translation.FromTree,
  )
where

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
  AnomaRpc :: Value -> Anoma m Value

makeSem ''Anoma

newtype AnomaPath = AnomaPath {_anomaPath :: Path Abs Dir}

newtype GrpcPort = GrpcPort {_grpcPort :: Int}

makeLenses ''AnomaPath
makeLenses ''GrpcPort

listenPort :: Int
listenPort = 50051

withSpawnAnomaClient ::
  (Members '[Process, Logger, EmbedIO, Reader AnomaPath, Reader GrpcPort] r) =>
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
      _ -> error "Something went wrong when starting the anoma client"
  where
    mkProcess :: (Members '[Reader AnomaPath, Reader GrpcPort] r') => Sem r' CreateProcess
    mkProcess = do
      p <- asks (^. anomaPath)
      grpcport <- asks (^. grpcPort)
      return
        ( proc
            (toFilePath (p <//> clientRelFile))
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

    -- Relative to the anoma repository
    clientRelFile :: Path Rel File
    clientRelFile = $(mkRelFile "apps/anoma_client/anoma_client")

withSapwnAnomaNode ::
  (Members '[EmbedIO, Logger, Process, Reader AnomaPath] r) =>
  (Int -> Handle -> ProcessHandle -> Sem r a) ->
  Sem r a
withSapwnAnomaNode body = withSystemTempFile "start.exs" $ \fp tmpHandle -> do
  liftIO (B.hPutStr tmpHandle anomaStartExs)
  hFlush tmpHandle
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

anomaRpc' :: (Members '[Reader AnomaPath, Process, EmbedIO] r) => Value -> Sem r Value
anomaRpc' msg = do
  cproc <- grpcCliProcess
  withCreateProcess cproc $ \mstdin mstdout _stderr _procHandle -> do
    let stdinH = fromJust mstdin
        stdoutH = fromJust mstdout
        inputbs = B.toStrict (encode msg)
    liftIO (B.hPutStr stdinH inputbs)
    hFlush stdinH
    res <- eitherDecodeStrict <$> liftIO (B.hGetContents stdoutH)
    case res of
      Right r -> return r
      Left err -> error (pack err)

grpcCliProcess :: (Members '[Reader AnomaPath] r) => Sem r CreateProcess
grpcCliProcess = do
  p <- asks (^. anomaPath)
  return
    ( proc
        "grpc_cli"
        [ "call",
          "--json_input",
          "--json_output",
          "--protofiles",
          toFilePath (p <//> relProtoFile),
          "localhost:" <> show listenPort,
          "Anoma.Protobuf.Intents.Prove"
        ]
    )
      { std_in = CreatePipe,
        std_out = CreatePipe
      }
  where
    relProtoFile :: Path Rel File
    relProtoFile = $(mkRelFile "apps/anoma_protobuf/priv/protobuf/anoma.proto")

runAnoma :: forall r a. (Members '[Logger, EmbedIO] r) => AnomaPath -> Sem (Anoma ': r) a -> Sem r a
runAnoma anomapath body = runReader anomapath . runConcurrent . runProcess $
  withSapwnAnomaNode $ \grpcport _nodeOut nodeH ->
    runReader (GrpcPort grpcport) $
      withSpawnAnomaClient $ \_clientH -> do
        (`interpret` inject body) $ \case
          NoHalt -> waitForProcess nodeH
          AnomaRpc i -> anomaRpc' i
