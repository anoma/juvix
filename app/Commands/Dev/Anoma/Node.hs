module Commands.Dev.Anoma.Node where

import Commands.Base
import Commands.Dev.Anoma.Node.Options
import Data.ByteString qualified as B
import Juvix.Extra.Paths (anomaStartExs)
import Prelude (read)

-- | Returns (stdout, processHandle)
withSapwnAnomaNode ::
  (Members '[EmbedIO, Process] r) =>
  Path Abs File ->
  Handle ->
  (Handle -> ProcessHandle -> Sem r a) ->
  Sem r a
withSapwnAnomaNode tmpFp tmpHandle body = do
  liftIO (B.hPutStr tmpHandle anomaStartExs)
  hFlush tmpHandle
  withCreateProcess (cprocess (toFilePath tmpFp)) $ \_stdin mstdout _stderr procHandle -> do
    body (fromJust mstdout) procHandle
  where
    cprocess :: FilePath -> CreateProcess
    cprocess exs =
      (proc "mix" ["run", "--no-halt", exs])
        { std_out = CreatePipe,
          -- This needs to be set to True so that Ctrl+c is handled by the
          -- elixir shell. If set to False, the elixir shell will be kept alive
          -- even if juvix is terminated
          delegate_ctlc = True
        }

-- | Echoes elixir output. Useful to see the help message when Ctrl+c is issued
echoHandle :: (Members '[EmbedIO] r) => Handle -> Sem r ()
echoHandle h = do
  theEnd <- hIsEOF h
  if
      | theEnd -> return ()
      | otherwise -> do
          ln <- hGetLine h
          putStrLn ln
          echoHandle h

listenPort :: Int
listenPort = 50051

withSpawnAnomaClient :: (Members '[Process, EmbedIO] r) => Int -> (ProcessHandle -> Sem r ()) -> Sem r ()
withSpawnAnomaClient grpcPort body =
  withCreateProcess cprocess $ \_stdin mstdout _stderr procHandle -> do
    let out = fromJust mstdout
    txt <- hGetLine out
    case takeWhile (/= '.') (unpack txt) of
      "Connected to node" -> do
        putStrLn "anoma client successfully started"
        body procHandle
      _ -> error "Something went wrong when starting the anoma client"
  where
    cprocess :: CreateProcess
    cprocess =
      ( proc
          (toFilePath clientRelFile)
          [ "--listen-port",
            show listenPort,
            "--node-host",
            "localhost",
            "--node-port",
            show grpcPort
          ]
      )
        { std_out = CreatePipe
        }

    -- Relative to the anoma repository
    clientRelFile :: Path Rel File
    clientRelFile = $(mkRelFile "apps/anoma_client/anoma_client")

runCommand :: forall r. (Members AppEffects r) => NodeOptions -> Sem r ()
runCommand opts = runConcurrent . runProcess $ do
  anomaDir :: Path Abs Dir <- fromAppPathDir (opts ^. nodeAnomaPath)
  withCurrentDir anomaDir $ do
    withSystemTempFile "start.exs" $ \fp tmpHandle ->
      withSapwnAnomaNode fp tmpHandle $ \nodeOut nodeH -> do
        grpcNode :: Int <- read . unpack <$> hGetLine nodeOut
        putStrLn ("grpc_node = " <> prettyText grpcNode)
        withAsync (echoHandle nodeOut) $ \_asyncH ->
          withSpawnAnomaClient grpcNode $ \_clientH -> do
            void (waitForProcess nodeH)
