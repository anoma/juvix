module Juvix.Data.Effect.Process.IO where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding.Error
import Juvix.Data.Effect.Process.Base
import Juvix.Prelude
import System.Process.Typed qualified as P

runProcessIO ::
  forall r a.
  (Members '[Embed IO] r) =>
  Sem (Process ': r) a ->
  Sem r a
runProcessIO = interpret $ \case
  FindExecutable' n -> findExecutable n
  ReadProcess' call -> do
    let p = P.proc (toFilePath (call ^. processCallPath)) (T.unpack <$> call ^. processCallArgs)
    (exitCode, stdoutRes, stderrRes) <- P.readProcess p
    return
      ProcessResult
        { _processResultExitCode = exitCode,
          _processResultStdout = toText stdoutRes,
          _processResultStderr = toText stderrRes
        }
  where
    toText :: LBS.ByteString -> Text
    toText lbs = decodeUtf8With lenientDecode (LBS.toStrict lbs)
