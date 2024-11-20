module Anoma.Client.Base where

import Data.Text qualified as T
import Juvix.Data.CodeAnn
import Juvix.Extra.Paths (anomaStartExs)
import Juvix.Prelude

data AnomaClientInfo = AnomaClientInfo
  { _anomaClientInfoPort :: Int,
    _anomaClientInfoUrl :: String
  }

newtype AnomaPath = AnomaPath {_anomaPath :: Path Abs Dir}

newtype AnomaProcess = AnomaProcess
  { _anomaProcessHandle :: ProcessHandle
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

anomaClientCreateProcess :: forall r. (Members '[Reader AnomaPath] r) => LaunchMode -> Sem r CreateProcess
anomaClientCreateProcess launchMode = do
  p <- baseProc
  return $ case launchMode of
    LaunchModeAttached -> p
    LaunchModeDetached -> p {new_session = True, std_err = NoStream}
  where
    baseProc :: Sem r CreateProcess
    baseProc = do
      anomapath <- asks (^. anomaPath)
      return
        (proc "mix" ["run", "--no-halt", "-e", unpack (T.strip (decodeUtf8 anomaStartExs))])
          { std_out = CreatePipe,
            cwd = Just (toFilePath anomapath),
            std_in = NoStream
          }

setupAnomaClientProcess :: (Members '[EmbedIO, Logger, Error SimpleError] r) => Handle -> Sem r AnomaClientInfo
setupAnomaClientProcess nodeOut = do
  ln <- hGetLine nodeOut
  let parseError = throw (SimpleError (mkAnsiText ("Failed to parse the client grpc port when starting the anoma node and client.\nExpected a number but got " <> ln)))
  grpcPort :: Int <- either (const parseError) return . readEither . unpack $ ln
  logInfo "Anoma node and client successfully started"
  logInfo (mkAnsiText ("Listening on port " <> annotate AnnImportant (pretty grpcPort)))
  return
    ( AnomaClientInfo
        { _anomaClientInfoPort = grpcPort,
          _anomaClientInfoUrl = "localhost"
        }
    )

launchAnomaClient :: (Members '[Logger, EmbedIO, Error SimpleError] r) => LaunchMode -> AnomaPath -> Sem r ProcessHandle
launchAnomaClient launchMode anomapath = runReader anomapath . runProcess $ do
  cproc <- anomaClientCreateProcess launchMode
  (_mstdin, mstdout, _mstderr, procHandle) <- createProcess cproc
  let stdoutH = fromJust mstdout
  setupAnomaClientProcess stdoutH
  hClose stdoutH
  return procHandle
