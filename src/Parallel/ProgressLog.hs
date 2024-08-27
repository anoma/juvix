module Parallel.ProgressLog where

import Juvix.Data.CodeAnn
import Juvix.Data.Logger
import Juvix.Prelude.Base

data ProgressLog :: Effect where
  ProgressLog :: LogItem -> ProgressLog m ()

newtype ProgressLogOptions = ProgressLogOptions
  { _progressLogOptionsShowThreadId :: Bool
  }

data LogItem = LogItem
  { _logItemThreadId :: ThreadId,
    _logItemMessage :: Doc CodeAnn
  }

makeSem ''ProgressLog
makeLenses ''ProgressLogOptions
makeLenses ''LogItem

defaultProgressLogOptions :: ProgressLogOptions
defaultProgressLogOptions =
  ProgressLogOptions
    { _progressLogOptionsShowThreadId = False
    }

runProgressLog :: (Members '[Logger] r) => ProgressLogOptions -> Sem (ProgressLog ': r) a -> Sem r a
runProgressLog ProgressLogOptions {..} = interpret $ \case
  ProgressLog LogItem {..} -> do
    let threadDoc :: Maybe (Doc CodeAnn) = do
          guard _progressLogOptionsShowThreadId
          return (kwBracketL <> show _logItemThreadId <> kwBracketR)
    logProgress (mkAnsiText (threadDoc <?+> _logItemMessage))

ignoreProgressLog :: Sem (ProgressLog ': r) a -> Sem r a
ignoreProgressLog = interpret $ \case
  ProgressLog {} -> return ()
