module Anoma.Client.Base where

import Data.Text qualified as T
import Effectful.Process (CreateProcess (env))
import Juvix.Data.CodeAnn
import Juvix.Extra.Paths (anomaStartExs)
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

data AnomaClientInfo = AnomaClientInfo
  { _anomaClientInfoPort :: Int,
    _anomaClientInfoUrl :: String,
    _anomaClientInfoNodeId :: Text
  }

$( deriveJSON
     Aeson.defaultOptions
       { unwrapUnaryRecords = True,
         allowOmittedFields = False,
         rejectUnknownFields = True,
         fieldLabelModifier = \case
           "_anomaClientInfoUrl" -> "url"
           "_anomaClientInfoPort" -> "port"
           "_anomaClientInfoNodeId" -> "nodeid"
           _ -> impossibleError "All fields must be covered"
       }
     ''AnomaClientInfo
 )

newtype AnomaPath = AnomaPath {_anomaPath :: Path Abs Dir}

newtype AnomaProcess = AnomaProcess
  { _anomaProcessHandle :: ProcessHandle
  }

data AnomaClientLaunchInfo = AnomaClientLaunchInfo
  { _anomaClientLaunchInfoInfo :: AnomaClientInfo,
    _anomaClientLaunchInfoProcess :: AnomaProcess
  }

data LaunchMode
  = -- | Launch the client process attached to the parent
    LaunchModeAttached
  | -- | Launch the client process detached from the parent
    LaunchModeDetached
  deriving stock (Data)

makeLenses ''AnomaClientInfo
makeLenses ''AnomaPath
makeLenses ''AnomaProcess
makeLenses ''AnomaClientLaunchInfo

anomaClientCreateProcess :: forall r. (Members '[Reader AnomaPath, Environment] r) => LaunchMode -> Sem r CreateProcess
anomaClientCreateProcess launchMode = do
  p <- baseProc
  return $ case launchMode of
    LaunchModeAttached -> p
    LaunchModeDetached -> p {new_session = True, std_err = NoStream}
  where
    -- The Anoma client outputs log messages on the Mix info channel. We must
    -- suppress these to parse the port and node_id in the client output
    quietEnv :: (String, String)
    quietEnv = ("MIX_QUIET", "1")

    baseProc :: Sem r CreateProcess
    baseProc = do
      currentEnv <- getEnvironment
      anomapath <- asks (^. anomaPath)
      return
        (proc "mix" ["phx.server", "--no-halt", "-e", unpack (T.strip (decodeUtf8 anomaStartExs))])
          { std_out = CreatePipe,
            std_err = CreatePipe,
            std_in = NoStream,
            cwd = Just (toFilePath anomapath),
            env = Just (quietEnv : currentEnv)
          }

setupAnomaClientProcess :: forall r. (Members '[EmbedIO, Logger, Error SimpleError] r) => Handle -> Sem r AnomaClientInfo
setupAnomaClientProcess nodeOut = do
  let x :: IO Text = catchIOError (readClientInfo nodeOut) $ \e -> error ("The node client is expected to output two numbers (see include/anoma/start.exs), but there was an IO exception:\n" <> show e)
  ln <- liftIO x
  let parseError = throw (SimpleError (mkAnsiText ("Failed to parse the client grpc port when starting the anoma node and client.\nExpected a number but got " <> ln)))
      parseInt :: Text -> Sem r Int
      parseInt = either (const parseError) return . readEither . unpack
  (grpcPort, nodeId) <- case T.words ln of
    [grpcPortStr, nodeIdStr] -> (,) <$> parseInt grpcPortStr <*> pure nodeIdStr
    _ -> throw (SimpleError (mkAnsiText ("Could not parse Anoma client output. Expected <grpcPort> <node_id>, got: " <> ln)))
  logInfo "Anoma node and client successfully started"
  logInfo (mkAnsiText ("Node ID: " <> annotate AnnImportant (pretty nodeId)))
  logInfo (mkAnsiText ("Listening on port " <> annotate AnnImportant (pretty grpcPort)))
  return
    AnomaClientInfo
      { _anomaClientInfoPort = grpcPort,
        _anomaClientInfoUrl = "localhost",
        _anomaClientInfoNodeId = nodeId
      }
  where
    readClientInfo :: Handle -> IO Text
    readClientInfo h = do
      ln <- hGetLine h
      case T.words ln of
        ("==>" : _) -> readClientInfo h
        _ -> return ln

launchAnomaClient :: (Members '[Logger, EmbedIO, Error SimpleError] r) => LaunchMode -> AnomaPath -> Sem r AnomaClientLaunchInfo
launchAnomaClient launchMode anomapath = runEnvironment . runReader anomapath . runProcess $ do
  cproc <- anomaClientCreateProcess launchMode
  (_mstdin, mstdout, _mstderr, procHandle) <- createProcess cproc
  let stdoutH = fromJust mstdout
  info <- setupAnomaClientProcess stdoutH
  hClose stdoutH
  return
    AnomaClientLaunchInfo
      { _anomaClientLaunchInfoInfo = info,
        _anomaClientLaunchInfoProcess = AnomaProcess procHandle
      }
