module Anoma.Client.Base where

import Data.Text qualified as T
import Juvix.Data.CodeAnn
import Juvix.Extra.Paths (anomaStartExs)
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

data AnomaClientInfo = AnomaClientInfo
  { _anomaClientInfoPort :: Int,
    _anomaClientInfoUrl :: String
  }

$( deriveJSON
     Aeson.defaultOptions
       { unwrapUnaryRecords = True,
         allowOmittedFields = False,
         rejectUnknownFields = True,
         fieldLabelModifier = \case
           "_anomaClientInfoUrl" -> "url"
           "_anomaClientInfoPort" -> "port"
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

launchAnomaClient :: (Members '[Logger, EmbedIO, Error SimpleError] r) => LaunchMode -> AnomaPath -> Sem r AnomaClientLaunchInfo
launchAnomaClient launchMode anomapath = runReader anomapath . runProcess $ do
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
