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

data ProcessE :: Effect where
  FindExecutable' :: Path Rel File -> ProcessE m (Maybe (Path Abs File))
  ReadProcess' :: ProcessCall -> ProcessE m ProcessResult

makeSem ''ProcessE
makeLenses ''ProcessResult
makeLenses ''ProcessCall
