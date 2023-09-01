module Juvix.Data.Effect.Process.Base where

import Juvix.Prelude

data ProcessResult = ProcessResult
  { _processResultExitCode :: ExitCode,
    _processResultStdout :: Text,
    _processResultStderr :: Text
  }

data ProcessCall = ProcessCall
  { _processCallPath :: Path Abs File,
    _processCallArgs :: [Text]
  }

data Process m a where
  FindExecutable' :: Path Rel File -> Process m (Maybe (Path Abs File))
  ReadProcess' :: ProcessCall -> Process m ProcessResult

makeSem ''Process
makeLenses ''ProcessResult
makeLenses ''ProcessCall
